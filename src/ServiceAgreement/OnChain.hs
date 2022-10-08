{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}

module ServiceAgreement.OnChain 
    ( ThreadToken
    , SM.getThreadToken
    , machineClient
    , typedValidator
    , valHash
    ) where 

import Ledger (ScriptContext)
import Plutus.V1.Ledger.Api (ValidatorHash)
import Ledger.Constraints qualified as Constraints
import Ledger.Constraints.TxConstraints (TxConstraints)
import Ledger.Interval qualified as Interval
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.Contract.StateMachine (State (..), StateMachine (..), StateMachineClient, ThreadToken, Void)
import Plutus.Contract.StateMachine qualified as SM
import PlutusTx qualified
import PlutusTx.Prelude hiding (check)

import ServiceAgreement.Types


{-# INLINABLE serviceAgreementTransition #-}
-- | The transitions of the service agreement state machine.
serviceAgreementTransition
    :: ServiceAgreementParams
    -> State ServiceAgreementState
    -> ServiceAgreementInput
    -> Maybe (TxConstraints Void Void, State ServiceAgreementState)
serviceAgreementTransition ServiceAgreementParams{..} State{stateData=oldStateData, stateValue=oldStateValue} input =
    case (oldStateData, input) of

        (OfferOpened _jobDoneDeadline, WithdrawOffer) -> 
            let constraints = Constraints.mustPayToPubKey sapCustomer oldStateValue
                           <> Constraints.mustValidateIn (Interval.from $ sapOfferAcceptanceDeadline)
                newState = State 
                    { stateData = Finished
                    , stateValue = mempty 
                    }
            in Just (constraints, newState)

        (OfferOpened jobDoneDeadline, AcceptOffer serviceProvider) | oldStateValue == sapPrice <> sapDisputeFee ->
            let constraints = Constraints.mustValidateIn (Interval.to $ sapOfferAcceptanceDeadline -1)
                           <> Constraints.mustBeSignedBy serviceProvider
                newState = State
                    { stateData = OfferAccepted jobDoneDeadline serviceProvider
                    , stateValue = oldStateValue <> sapServiceProviderCollateral
                    }
            in Just (constraints, newState)

        (OfferAccepted jobDoneDeadline serviceProvider, ExtendJobDoneDeadline newJobDoneDeadline) | newJobDoneDeadline > jobDoneDeadline -> 
            let constraints = Constraints.mustValidateIn (Interval.to $ jobDoneDeadline -1)
                           <> Constraints.mustBeSignedBy sapCustomer
                newState = State
                    { stateData = OfferAccepted newJobDoneDeadline serviceProvider
                    , stateValue = oldStateValue
                    }
            in Just (constraints, newState)       

        (OfferAccepted _jobDoneDeadline serviceProvider, ClaimJobDone _jobDoneProof) -> 
            let constraints = Constraints.mustPayToPubKey serviceProvider (sapPrice <> sapServiceProviderCollateral) 
                           <> Constraints.mustPayToPubKey sapCustomer sapDisputeFee
                newState = State 
                    { stateData = Finished
                    , stateValue = mempty 
                    }
            in Just (constraints, newState)

        (OfferAccepted _jobDoneDeadline serviceProvider, OpenDispute) -> 
            let constraints = Constraints.mustBeSignedBy serviceProvider
                newState = State 
                    { stateData = InDispute serviceProvider
                    , stateValue = oldStateValue <> sapDisputeFee 
                    }
            in Just (constraints, newState)        

        (InDispute serviceProvider, ResolveDispute (ServiceProviderWins serviceProvider')) | serviceProvider == serviceProvider' ->
            let constraints = Constraints.mustBeSignedBy sapDisputeProvider 
                           <> Constraints.mustPayToPubKey serviceProvider (sapPrice <> sapServiceProviderCollateral <> sapDisputeFee)
                           <> Constraints.mustPayToPubKey sapDisputeProvider sapDisputeFee
                newState = State 
                    { stateData = Finished
                    , stateValue = mempty
                    }
            in Just (constraints, newState)  

        (InDispute _serviceProvider, ResolveDispute (CustomerWins customer)) | sapCustomer == customer ->
            let constraints = Constraints.mustBeSignedBy sapDisputeProvider 
                           <> Constraints.mustPayToPubKey sapCustomer (sapPrice <> sapServiceProviderCollateral <> sapDisputeFee)
                           <> Constraints.mustPayToPubKey sapDisputeProvider sapDisputeFee
                newState = State 
                    { stateData = Finished
                    , stateValue = mempty
                    }
            in Just (constraints, newState)  


        (OfferAccepted jobDoneDeadline _serviceProvider, ClaimJobNotDone) -> 
            let constraints = Constraints.mustValidateIn (Interval.from jobDoneDeadline)
                           <> Constraints.mustPayToPubKey sapCustomer (sapPrice <> sapServiceProviderCollateral <> sapDisputeFee) 
                newState = State 
                    { stateData = Finished
                    , stateValue = mempty 
                    }
            in Just (constraints, newState)          

        -- Any other combination of 'ServiceAgreementState' and 'ServiceAgreementInput' is disallowed.
        _ -> Nothing

-- TODO DOC
-- sha2_256 (JobDoneNonce `appendByteString` JobDoneProof) == JobDoneVerificator
{-# INLINABLE check #-}
check :: JobDoneNonce -> JobDoneVerificator -> ServiceAgreementState -> ServiceAgreementInput -> ScriptContext -> Bool
check jobDoneNonce jobDoneVerificator (OfferAccepted _jobDoneDeadline _serviceProvider) (ClaimJobDone jobDoneProof) _ctx =
    sha2_256 (jobDoneNonce `appendByteString` jobDoneProof) == jobDoneVerificator
check _ _ _ _ _ = True

{-# INLINABLE final #-}
final :: ServiceAgreementState -> Bool
final Finished = True
final _        = False

{-# INLINABLE serviceAgreementStateMachine #-}
serviceAgreementStateMachine :: (ThreadToken, ServiceAgreementParams) -> ServiceAgreementMachine
serviceAgreementStateMachine (threadToken, params@ServiceAgreementParams{..}) = StateMachine
    { smTransition  = serviceAgreementTransition params
    , smFinal       = final
    , smCheck       = check sapJobDoneNonce sapJobDoneVerificator
    , smThreadToken = Just threadToken
    }

{-# INLINABLE mkValidator #-}
mkValidator :: (ThreadToken, ServiceAgreementParams) -> Scripts.ValidatorType ServiceAgreementMachine
mkValidator = SM.mkValidator . serviceAgreementStateMachine

typedValidator :: (ThreadToken, ServiceAgreementParams) -> Scripts.TypedValidator ServiceAgreementMachine
typedValidator = Scripts.mkTypedValidatorParam @ServiceAgreementMachine
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.mkUntypedValidator

valHash :: (ThreadToken, ServiceAgreementParams) -> ValidatorHash
valHash = Scripts.validatorHash . typedValidator

machineClient
    :: Scripts.TypedValidator ServiceAgreementMachine
    -> ThreadToken -- ^ Thread token of the instance
    -> ServiceAgreementParams
    -> StateMachineClient ServiceAgreementState ServiceAgreementInput
machineClient inst threadToken serviceAgreementParams =
    let machine = serviceAgreementStateMachine (threadToken, serviceAgreementParams)
    in SM.mkStateMachineClient (SM.StateMachineInstance machine inst)