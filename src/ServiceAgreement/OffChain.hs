{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module ServiceAgreement.OffChain where
    
import Control.Lens (makeClassyPrisms)
import Data.Aeson (FromJSON, ToJSON)
import Data.Monoid (Last (..))
import Data.Semigroup.Generic (GenericSemigroupMonoid (..))
import GHC.Generics (Generic)
import Ledger (POSIXTime(..), Value)
import Plutus.Contract
import Plutus.Contract.StateMachine (StateMachineClient, WaitingResult (..))
import Plutus.Contract.StateMachine qualified as SM
import Plutus.Contract.Util (loopM)
import PlutusTx.Prelude
import Prelude qualified as Haskell

import ServiceAgreement.OnChain
import ServiceAgreement.Types


type CustomerSchema        = Endpoint "extend-job-done-deadline" JobDoneDeadline
                         .\/ Endpoint "withdraw-offer" ()     -- This endpoint is provided only for testing negative use cases
                         .\/ Endpoint "claim-job-not-done" () -- This endpoint is provided only for testing negative use cases

type ServiceProviderSchema =  Endpoint "accept-offer" ()
                          .\/ Endpoint "claim-job-done" JobDoneProof
                          .\/ Endpoint "open-dispute" ()
                          .\/ Endpoint "wait" () 

type DisputeProviderSchema = Endpoint "resolve-dispute" DisputeResolution

data AgreementResult 
    = OfferNotAccepted
    | JobDone
    | JobNotDoneOntime
    | DisputeResolved
    deriving stock (Generic, Haskell.Show, Haskell.Eq)
    deriving anyclass (ToJSON, FromJSON)

data ServiceAgreementLog 
    = OfferOpenedLog
    | ServiceAgreementFailedLog SM.SMContractError
    | OfferAcceptedLog ServiceProvider
    | CustomerWantsToExtendJobDoneDeadlineLog JobDoneDeadline
    | JobDoneDeadlineExtendedLog JobDoneDeadline
    | OfferAcceptanceDeadlineReachedWithoutOfferAcceptedLog
    | JobDoneDeadlineReachedWithoutJobDoneOnTimeLog
    | ServiceProviderWantsToOpenADisputeLog
    | InDisputeLog
    | DisputeProviderHasNotResolvedLog
    | DisputeServiceWantsToResolveLog
    | FinishedLog AgreementResult

    | ProviderWantsToAcceptOfferLog
    | ProviderWantsClaimJobDoneLog JobDoneProof

    | CurrentStateNotFoundLog
    | TransitionFailedLog (SM.InvalidTransition ServiceAgreementState ServiceAgreementInput)
    | RunStepDoneLog 
    | RunWaitForUpdateLog
    | NonSenseStateLog (Maybe ServiceAgreementState)
    deriving stock (Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data ServiceAgreementError 
    = StateMachineContractError SM.SMContractError -- ^ State machine operation failed
    | ServiceAgreementContractError ContractError -- ^ Endpoint, coin selection, etc. failed
    | NoSenseState
    deriving stock (Haskell.Eq, Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

makeClassyPrisms ''ServiceAgreementError

instance AsContractError ServiceAgreementError where
    _ContractError = _ServiceAgreementContractError . _ContractError

instance SM.AsSMContractError ServiceAgreementError where
    _SMContractError = _StateMachineContractError . SM._SMContractError    

data CustomerOutput =
    CustomerOutput
        { coState       :: Last ServiceAgreementState
        , coThreadToken :: Last ThreadToken
        , coParams      :: Last ServiceAgreementParams
        }
        deriving stock (Generic, Haskell.Show, Haskell.Eq)
        deriving anyclass (ToJSON, FromJSON)
        deriving (Haskell.Semigroup, Haskell.Monoid) via (GenericSemigroupMonoid CustomerOutput)

customerStateOut :: ServiceAgreementState -> CustomerOutput
customerStateOut s = Haskell.mempty { coState = Last (Just s) }

customerThreadTokenOut :: ThreadToken -> CustomerOutput
customerThreadTokenOut t = Haskell.mempty { coThreadToken = Last (Just t) }

customerParamsOut :: ServiceAgreementParams -> CustomerOutput
customerParamsOut params = Haskell.mempty { coParams = Last (Just params) }

data ServiceProviderOutput = ServiceProviderOutput { spoState :: Last ServiceAgreementState }
    deriving stock (Generic, Haskell.Show, Haskell.Eq)
    deriving anyclass (ToJSON, FromJSON)
    deriving (Haskell.Semigroup, Haskell.Monoid) via (GenericSemigroupMonoid ServiceProviderOutput)

serviceProviderStateOut :: ServiceAgreementState -> ServiceProviderOutput
serviceProviderStateOut s = Haskell.mempty { spoState = Last (Just s) }        

-- | Client code for the customer
customerContract :: BuiltinByteString
                         -> POSIXTime 
                         -> POSIXTime 
                         -> DisputeProvider 
                         -> JobDoneVerificator
                         -> JobDoneNonce 
                         -> Value 
                         -> Value 
                         -> Value
                         -> Contract CustomerOutput CustomerSchema ServiceAgreementError ()
customerContract agreement offerAcceptanceDeadline jobDoneDeadline disputeProvider jobDoneVerificator jobDoneNonce price disputeFee serviceProviderCollateral = do
    threadToken <- SM.getThreadToken
    tell $ customerThreadTokenOut threadToken
    self <- ownFirstPaymentPubKeyHash
    let params       = ServiceAgreementParams 
                           { sapCustomer                  = self
                           , sapAgreement                 = agreement
                           , sapPrice                     = price 
                           , sapDisputeProvider           = disputeProvider
                           , sapOfferAcceptanceDeadline   = offerAcceptanceDeadline
                           , sapJobDoneVerificator        = jobDoneVerificator 
                           , sapJobDoneNonce              = jobDoneNonce
                           , sapDisputeFee                = disputeFee
                           , sapServiceProviderCollateral = serviceProviderCollateral
                           }

        inst         = typedValidator (threadToken, params)
        client       = machineClient inst threadToken params
        state0       = initialState jobDoneDeadline
        loop         = loopM (\ctx -> waitForChangeCustomer client ctx >>= handleEventCustomer client ctx)

    _ <- handleError
            (\e -> do { logError (ServiceAgreementFailedLog e); throwError (StateMachineContractError e) })
            (SM.runInitialise client state0 (price <> disputeFee))
    
    tell $ customerParamsOut params
    tell $ customerStateOut state0
    logInfo $ OfferOpenedLog
    loop $ CustomerContext { ccDeadlineToWait = WaitForOfferAcceptance offerAcceptanceDeadline 
                           , ccState          = COSOfferNotAccepted
                           }


data CustomerEvent 
    = CEOfferAcceptanceDeadlineReachedWithoutOfferAcceptedEvent
    | CEJobDoneDeadlineReachedWithoutJobDoneOnTimeEvent
    | CEAlmostInfiniteDeadlineReachedEvent
    | CEJobDoneDeadlineExtendedEvent JobDoneDeadline
    | CEFinishedEvent 
    | CEOfferAcceptedEvent JobDoneDeadline ServiceProvider
    | CEInDisputeEvent ServiceProvider


data CustomerOffchainState 
    = COSOfferNotAccepted 
    | COSOfferAccepted
    | COSDisputeOpened    

data CustomerContext = CustomerContext 
    { ccDeadlineToWait :: DeadlineToWait 
    , ccState          :: CustomerOffchainState
    }  

data DeadlineToWait 
    = WaitForOfferAcceptance POSIXTime
    | WaitForJobDone POSIXTime
    | NoDeadline
  
waitForChangeCustomer
    :: StateMachineClient ServiceAgreementState ServiceAgreementInput
    -> CustomerContext
    -> Contract CustomerOutput CustomerSchema ServiceAgreementError CustomerEvent
waitForChangeCustomer client CustomerContext{ ccDeadlineToWait } = do
    -- Create a Promise that waits for either an update to the state machine's
    -- on chain state, or until the deadline provided (this will be either offerAcceptanceDeadline or jobDoneDeadline)

    let infiniteDeadLine = POSIXTime $ Haskell.toInteger (Haskell.maxBound :: Haskell.Int)

    smUpdatePromise <- SM.waitForUpdateTimeout client $ case ccDeadlineToWait of 
        WaitForOfferAcceptance offerAcceptanceDeadline -> isTime offerAcceptanceDeadline
        WaitForJobDone jobDoneDeadline                 -> isTime jobDoneDeadline
        NoDeadline                                     -> isTime infiniteDeadLine

    let withdrawOffer         = endpoint @"withdraw-offer"           $ const $ pure CEOfferAcceptanceDeadlineReachedWithoutOfferAcceptedEvent
        claimJobNotDone       = endpoint @"claim-job-not-done"       $ const $ pure CEJobDoneDeadlineReachedWithoutJobDoneOnTimeEvent
        extendJobDoneDeadline = endpoint @"extend-job-done-deadline" $ pure . CEJobDoneDeadlineExtendedEvent
        otherEvent = do
            promiseBind
                smUpdatePromise
                $ \case
                    -- If the state machine instance ended, then the agreement has ended.
                    -- In this case match, 'currentState client' should always be
                    -- 'Nothing'.
                    ContractEnded {} -> pure CEFinishedEvent
                    -- The state machine transitionned to a new state
                    Transition {} -> do
                        optState <- currentState client
                        case optState of
                            -- If there is no current state, then the service agreement has ended.
                            Nothing                                              -> throwError NoSenseState  -- pure CEFinishedEvent
                            Just (OfferOpened _)                                 -> throwError NoSenseState  -- pure $ OfferOpenedEvent d
                            Just Finished                                        -> pure $ CEFinishedEvent
                            Just (OfferAccepted jobDoneDeadline serviceProvider) -> pure $ CEOfferAcceptedEvent jobDoneDeadline serviceProvider
                            Just (InDispute serviceProvider)                     -> pure $ CEInDisputeEvent serviceProvider

                    Timeout _t -> case ccDeadlineToWait of 
                        WaitForOfferAcceptance _ -> pure CEOfferAcceptanceDeadlineReachedWithoutOfferAcceptedEvent
                        WaitForJobDone _         -> pure CEJobDoneDeadlineReachedWithoutJobDoneOnTimeEvent
                        NoDeadline               -> pure CEAlmostInfiniteDeadlineReachedEvent

                    InitialState _ -> throwError NoSenseState

    selectList [extendJobDoneDeadline, otherEvent, withdrawOffer, claimJobNotDone]


handleEventCustomer
    :: StateMachineClient ServiceAgreementState ServiceAgreementInput
    -> CustomerContext
    -> CustomerEvent
    -> Contract CustomerOutput CustomerSchema ServiceAgreementError (Either CustomerContext ())
handleEventCustomer client ctx@CustomerContext{..} event =
    let continue = pure . Left
        stop     = pure (Right ())
    in case event of
        CEFinishedEvent -> do 
            let result = case ccState of 
                    COSOfferNotAccepted -> OfferNotAccepted
                    COSDisputeOpened    -> DisputeResolved
                    COSOfferAccepted    -> JobDone

            logInfo $ FinishedLog result
            tell $ customerStateOut Finished
            stop

        CEJobDoneDeadlineExtendedEvent newJobDoneDeadline -> do
            logInfo $ CustomerWantsToExtendJobDoneDeadlineLog newJobDoneDeadline 
            r <- SM.runStep client $ ExtendJobDoneDeadline newJobDoneDeadline
            logInfo RunStepDoneLog
            case r of
                SM.TransitionFailure i -> do 
                    logError (TransitionFailedLog i)
                    continue ctx

                SM.TransitionSuccess (OfferAccepted newJobDoneDeadline' serviceProvider) -> do 
                    logInfo $ JobDoneDeadlineExtendedLog newJobDoneDeadline'
                    tell $ customerStateOut (OfferAccepted newJobDoneDeadline' serviceProvider)
                    continue $ ctx { ccDeadlineToWait = WaitForJobDone newJobDoneDeadline' }

                SM.TransitionSuccess s -> do
                    logError $ NonSenseStateLog (Just s)
                    throwError NoSenseState

        CEOfferAcceptanceDeadlineReachedWithoutOfferAcceptedEvent -> do
            logInfo OfferAcceptanceDeadlineReachedWithoutOfferAcceptedLog
            r <- SM.runStep client WithdrawOffer
            logInfo RunStepDoneLog
            case r of
                SM.TransitionFailure i -> do 
                    logError (TransitionFailedLog i)
                    stop 

                SM.TransitionSuccess Finished -> do
                    logInfo $ FinishedLog OfferNotAccepted
                    tell $ customerStateOut Finished
                    stop

                SM.TransitionSuccess s -> do
                    logError $ NonSenseStateLog (Just s)
                    throwError NoSenseState

        CEJobDoneDeadlineReachedWithoutJobDoneOnTimeEvent -> do 
            logInfo JobDoneDeadlineReachedWithoutJobDoneOnTimeLog
            r <- SM.runStep client ClaimJobNotDone
            logInfo RunStepDoneLog
            case r of
                SM.TransitionFailure i -> do 
                    logError (TransitionFailedLog i)
                    continue ctx 

                SM.TransitionSuccess Finished -> do 
                    logInfo $ FinishedLog JobNotDoneOntime
                    tell $ customerStateOut Finished
                    stop

                SM.TransitionSuccess s -> do
                    logError $ NonSenseStateLog (Just s)
                    throwError NoSenseState

        CEOfferAcceptedEvent jobDoneDeadline serviceProvider -> do
            logInfo $ OfferAcceptedLog serviceProvider
            tell $ customerStateOut (OfferAccepted jobDoneDeadline serviceProvider)
            continue $ ctx { ccDeadlineToWait = WaitForJobDone jobDoneDeadline
                           , ccState          = COSOfferAccepted
                           } 

        CEInDisputeEvent serviceProvider -> do 
            logInfo InDisputeLog
            tell $ customerStateOut (InDispute serviceProvider)
            continue $ ctx { ccDeadlineToWait = NoDeadline 
                           , ccState          = COSDisputeOpened
                           } 

        CEAlmostInfiniteDeadlineReachedEvent -> do
            logError DisputeProviderHasNotResolvedLog
            stop 


serviceProviderContract :: ThreadToken -> ServiceAgreementParams -> Contract ServiceProviderOutput ServiceProviderSchema ServiceAgreementError ()
serviceProviderContract currency params = do
    let inst   = typedValidator (currency, params)
        client = machineClient inst currency params
        loop   = loopM (\ctx -> waitForChangeProvider client >>= handleEventProvider client ctx)

    initial <- currentState client
    self    <- ownFirstPaymentPubKeyHash
    case initial of
        Just (OfferOpened jobDoneDeadline) -> let ctx = ServiceProviderContext self jobDoneDeadline SPOSOfferNotAccepted
                                              in loop ctx

        Just _                             -> throwError NoSenseState

        -- If the state can't be found we wait for it to appear.
        Nothing -> SM.waitForUpdateUntilTime client (sapOfferAcceptanceDeadline params) >>= \case
            Transition _ (OfferOpened jobDoneDeadline) -> let ctx = ServiceProviderContext self jobDoneDeadline SPOSOfferNotAccepted
                                                          in loop ctx

            InitialState (OfferOpened jobDoneDeadline) -> let ctx = ServiceProviderContext self jobDoneDeadline SPOSOfferNotAccepted
                                                          in loop ctx

            _                                          -> logWarn CurrentStateNotFoundLog >> throwError NoSenseState


data ServiceProviderEvent 
    = SPEServiceProviderWantsToAcceptOfferEvent
    | SPEServiceProviderClaimsJobDoneEvent JobDoneProof
    | SPEServicdProviderWantsToOpenADisputeEvent 
    | SPEFinishedEvent
    | SPEJobDoneDeadlineExtendedEvent JobDoneDeadline

data ServiceProviderOffchainState 
    = SPOSOfferNotAccepted 
    | SPOSOfferAccepted
    | SPOSDisputeOpened

data ServiceProviderContext = ServiceProviderContext 
    { spcServiceProvider :: ServiceProvider
    , spcJobDoneDeadline :: JobDoneDeadline
    , spcState           :: ServiceProviderOffchainState
    } 

waitForChangeProvider
    :: StateMachineClient ServiceAgreementState ServiceAgreementInput
    -> Contract ServiceProviderOutput ServiceProviderSchema ServiceAgreementError ServiceProviderEvent
waitForChangeProvider client = do
    let infiniteDeadLine = POSIXTime $ Haskell.toInteger (Haskell.maxBound :: Haskell.Int)

    smUpdatePromise <- SM.waitForUpdateTimeout client (isTime infiniteDeadLine) -- TODOR Check with Roberto if exists a better approach

    let acceptOffer  = endpoint @"accept-offer"   $ const $ pure SPEServiceProviderWantsToAcceptOfferEvent
        claimJobDone = endpoint @"claim-job-done" $ pure . SPEServiceProviderClaimsJobDoneEvent
        openDispute  = endpoint @"open-dispute"   $ const $ pure SPEServicdProviderWantsToOpenADisputeEvent
        wait = do
            promiseBind
                smUpdatePromise
                $ \case
                    Transition {} -> do
                        optState <- currentState client
                        case optState of
                            Nothing                                                   -> throwError NoSenseState 
                            Just (OfferOpened _)                                      -> throwError NoSenseState 
                            Just Finished                                             -> pure SPEFinishedEvent
                            Just (OfferAccepted  newJobDoneDeadline _serviceProvider) -> pure $ SPEJobDoneDeadlineExtendedEvent newJobDoneDeadline
                            Just (InDispute _serviceProvider)                         -> throwError NoSenseState
                    
                    ContractEnded {} -> pure SPEFinishedEvent

                    _ -> throwError NoSenseState
                    
    selectList [acceptOffer, claimJobDone, openDispute, wait]            


handleEventProvider
    :: StateMachineClient ServiceAgreementState ServiceAgreementInput
    -> ServiceProviderContext
    -> ServiceProviderEvent
    -> Contract ServiceProviderOutput ServiceProviderSchema ServiceAgreementError (Either ServiceProviderContext ())
handleEventProvider client ctx@ServiceProviderContext{..} event =
    let continue = pure . Left
        stop     = pure (Right ())
    in case event of
        SPEFinishedEvent -> do 
            let result = case spcState of 
                    SPOSOfferNotAccepted -> OfferNotAccepted
                    SPOSDisputeOpened    -> DisputeResolved
                    SPOSOfferAccepted    -> JobNotDoneOntime

            logInfo $ FinishedLog result
            tell $ serviceProviderStateOut Finished
            stop
        
        SPEJobDoneDeadlineExtendedEvent newJobDoneDeadline -> do
            logInfo $ JobDoneDeadlineExtendedLog newJobDoneDeadline
            tell $ serviceProviderStateOut (OfferAccepted newJobDoneDeadline spcServiceProvider)
            continue $ ctx { spcJobDoneDeadline = newJobDoneDeadline }

        SPEServiceProviderWantsToAcceptOfferEvent -> do
            logInfo ProviderWantsToAcceptOfferLog
            r <- SM.runStep client (AcceptOffer spcServiceProvider)
            logInfo RunStepDoneLog
            case r of
                SM.TransitionFailure i -> do 
                    logError (TransitionFailedLog i)
                    continue ctx 

                SM.TransitionSuccess state@(OfferAccepted _ _) -> do
                    logInfo $ OfferAcceptedLog spcServiceProvider
                    tell $ serviceProviderStateOut state
                    continue $ ctx { spcState = SPOSOfferAccepted }

                SM.TransitionSuccess s -> do
                    logError $ NonSenseStateLog (Just s)
                    throwError NoSenseState

        SPEServiceProviderClaimsJobDoneEvent proof -> do 
            logInfo $ ProviderWantsClaimJobDoneLog proof
            r <- SM.runStep client (ClaimJobDone proof)
            logInfo RunStepDoneLog
            case r of
                SM.TransitionFailure i -> do 
                    logError (TransitionFailedLog i)
                    continue ctx 

                SM.TransitionSuccess Finished -> do   -- Warning: When proof is invalid I see a tx failing but it results in TransitionSuccess Finished
                    logInfo $ FinishedLog JobDone
                    tell $ serviceProviderStateOut Finished
                    stop

                SM.TransitionSuccess s -> do
                    logError $ NonSenseStateLog (Just s)
                    throwError NoSenseState

        SPEServicdProviderWantsToOpenADisputeEvent -> do 
            logInfo ServiceProviderWantsToOpenADisputeLog
            r <- SM.runStep client OpenDispute
            logInfo RunStepDoneLog
            case r of
                SM.TransitionFailure i -> do 
                    logError (TransitionFailedLog i)
                    continue ctx 

                SM.TransitionSuccess state@(InDispute _serviceProvider)-> do 
                    logInfo $ InDisputeLog 
                    tell $ serviceProviderStateOut state
                    continue $ ctx { spcState = SPOSDisputeOpened }

                SM.TransitionSuccess s -> do
                    logError $ NonSenseStateLog (Just s)
                    throwError NoSenseState


--- Dispute Provider ---

disputeProviderContract :: ThreadToken -> ServiceAgreementParams -> Contract () DisputeProviderSchema ServiceAgreementError ()
disputeProviderContract currency params = do
    let inst   = typedValidator (currency, params)
        client = machineClient inst currency params

    awaitPromise $ endpoint @"resolve-dispute" $ \resolution -> do
        logInfo $ DisputeServiceWantsToResolveLog
        r <- SM.runStep client $ ResolveDispute resolution
        logInfo RunStepDoneLog
        case r of
            SM.TransitionFailure i -> do 
                logError (TransitionFailedLog i)
            SM.TransitionSuccess Finished -> do 
                logInfo $ FinishedLog DisputeResolved
            SM.TransitionSuccess s -> do
                logError $ NonSenseStateLog (Just s)
                throwError NoSenseState


-- | Get the current state of the contract and log it.
currentState
    :: StateMachineClient ServiceAgreementState ServiceAgreementInput
    -> Contract a b ServiceAgreementError (Maybe ServiceAgreementState)
currentState client = do
  mOcs <- mapError StateMachineContractError (SM.getOnChainState client)
  case mOcs of
    Just (onChainState, _) -> do
        let s = SM.getStateData onChainState
        pure (Just s)
    _ -> do
        logWarn CurrentStateNotFoundLog
        pure Nothing


