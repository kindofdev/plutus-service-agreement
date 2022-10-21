{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Spec.ServiceAgreement
    ( tests
    , options
    , runMyTrace
    , custOpensAnOfferT
    , custWithdrawnOfferT
    , custWithdrawnOfferNegT
    , servProvAcceptsOfferT
    , servProvAcceptsOfferNegT
    , servProvAcceptsOfferCustNegFS
    , custClaimsJobNotDoneT
    , custClaimsJobNotDoneNegT
    , servProvJobOnTimeT
    , servProvJobOnTimeNegT
    , custExtendsJobDoneDeadlineT
    , servProvClaimsJobDoneAfterCustomerExtendsJobDoneDeadlineT
    , servProvOpensADisputeT
    , custClaimsJobDoneButDisputeOpenedNegT
    , custClaimsJobDoneButDisputeOpenedNegCustFS
    , dispProvDecidesCustWinsT
    , dispProvDecidesServProvWinsT
    ) where

import Control.Lens hiding (elements)
import Control.Monad (void)
import Control.Monad.Freer qualified as Freer
import Control.Monad.Freer.Error qualified as Freer
import Data.Default (Default (def))
import Data.Fixed
import Data.Monoid (Last (..))

import Ledger (Value, POSIXTime(..))
import Ledger.Ada qualified as Ada
import Ledger.TimeSlot (SlotConfig)
import Ledger.TimeSlot qualified as TimeSlot
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.Contract hiding (currentSlot)
import Plutus.Contract.StateMachine.ThreadToken qualified as TT
import Plutus.Contract.Test hiding (not)
import Plutus.Contract.Test.ContractModel
import Plutus.Trace.Emulator qualified as Trace
import PlutusTx.Prelude (sha2_256, appendByteString, BuiltinByteString)
import Streaming.Prelude qualified as S
import Wallet.Emulator.Folds qualified as Folds
import Wallet.Emulator.Stream qualified as Stream

import Test.Tasty

import ServiceAgreement.Types
import ServiceAgreement.OffChain
import ServiceAgreement.OnChain


slotCfg :: SlotConfig
slotCfg = def

options :: CheckOptions
options = defaultCheckOptionsContractModel
    & allowBigTransactions

jobDoneNonce :: JobDoneNonce
jobDoneNonce = "the_nonce"

jobDoneProof :: JobDoneProof
jobDoneProof = "secret_proof"

jobDoneVerificator :: JobDoneVerificator
jobDoneVerificator = sha2_256 (jobDoneNonce `appendByteString` jobDoneProof)

jobDoneDeadline :: JobDoneDeadline
jobDoneDeadline = TimeSlot.scSlotZeroTime slotCfg + 800_000

jobDoneDeadlineExtended :: JobDoneDeadline
jobDoneDeadlineExtended = TimeSlot.scSlotZeroTime slotCfg + 1_200_000

offerAcceptanceDeadline :: OfferAcceptanceDeadline
offerAcceptanceDeadline = TimeSlot.scSlotZeroTime slotCfg + 100_000

customer :: Customer
customer = mockWalletPaymentPubKeyHash w1

agreement :: BuiltinByteString
agreement = "This is the agreement."

disputeProvider :: DisputeProvider
disputeProvider = mockWalletPaymentPubKeyHash w3

serviceProvider :: DisputeProvider
serviceProvider = mockWalletPaymentPubKeyHash w2

price :: Data.Fixed.Micro
price = 100

valuePrice :: Value
valuePrice = Ada.adaValueOf price

disputeFee :: Data.Fixed.Micro
disputeFee = 10

valueDisputeFee :: Value
valueDisputeFee = Ada.adaValueOf disputeFee

serviceProviderCollateral :: Data.Fixed.Micro
serviceProviderCollateral = 15

valueServiceProviderCollateral :: Value
valueServiceProviderCollateral = Ada.adaValueOf serviceProviderCollateral

params' :: ServiceAgreementParams
params' = ServiceAgreementParams
    { sapCustomer = customer
    , sapAgreement = agreement
    , sapPrice = valuePrice 
    , sapDisputeProvider = disputeProvider
    , sapOfferAcceptanceDeadline = offerAcceptanceDeadline
    , sapJobDoneVerificator = jobDoneVerificator
    , sapJobDoneNonce = jobDoneNonce
    , sapDisputeFee = valueDisputeFee
    , sapServiceProviderCollateral = valueServiceProviderCollateral
    }

customerContract' :: Contract CustomerOutput CustomerSchema ServiceAgreementError ()
customerContract' = customerContract agreement
                                     offerAcceptanceDeadline 
                                     jobDoneDeadline
                                     disputeProvider 
                                     jobDoneVerificator
                                     jobDoneNonce
                                     valuePrice 
                                     valueDisputeFee
                                     valueServiceProviderCollateral 


extractAssetClass :: Trace.ContractHandle CustomerOutput CustomerSchema ServiceAgreementError -> Trace.EmulatorTrace ThreadToken
extractAssetClass handle = do
    t <- coThreadToken <$> Trace.observableState handle
    case t of
        Last (Just currency) -> pure currency
        _                    -> Trace.throwError (Trace.GenericError "currency not found")

extractParams :: Trace.ContractHandle CustomerOutput CustomerSchema ServiceAgreementError -> Trace.EmulatorTrace ServiceAgreementParams
extractParams handle = do
    t <- coParams <$> Trace.observableState handle
    case t of
        Last (Just params) -> pure params
        _                  -> Trace.throwError (Trace.GenericError "params not found")        

threadToken :: ThreadToken
threadToken =
    let con = getThreadToken :: Contract CustomerOutput CustomerSchema ServiceAgreementError ThreadToken
        fld = Folds.instanceOutcome con (Trace.walletInstanceTag w1)
        getOutcome (Folds.Done a) = a
        getOutcome e              = error $ "not finished: " <> show e
    in
    either (error . show) (getOutcome . S.fst')
        $ Freer.run
        $ Freer.runError @Folds.EmulatorFoldErr
        $ Stream.foldEmulatorStreamM fld
        $ Stream.takeUntilSlot 10
        $ Trace.runEmulatorStream (options ^. emulatorConfig)
        $ do
            void $ Trace.activateContractWallet w1 (void con)
            Trace.waitNSlots 3

threadTokenToValue :: TT.ThreadToken -> Value 
threadTokenToValue tt@(TT.ThreadToken _ cs) = TT.threadTokenValue cs $ valHash (tt, params')

--- customer opens an offer --- 

custOpensAnOfferT :: Trace.EmulatorTrace ()
custOpensAnOfferT = do
    void $ Trace.activateContractWallet w1 customerContract'
    void $ Trace.waitNSlots 3

custOpensAnOfferCustFS :: CustomerOutput
custOpensAnOfferCustFS = CustomerOutput
    { coState       = Last $ Just $ OfferOpened jobDoneDeadline
    , coThreadToken = Last $ Just threadToken
    , coParams      = Last $ Just params'
    }

------ customer withdrawn an offer - happy path --------

custWithdrawnOfferT :: Trace.EmulatorTrace ()
custWithdrawnOfferT = do
    customerHdl <- Trace.activateContractWallet w1 customerContract'
    void $ Trace.waitNSlots 1
    params <- extractParams customerHdl
    void $ Trace.waitUntilTime $ sapOfferAcceptanceDeadline params
    void $ Trace.waitNSlots 1

custWithdrawnOfferCustFS :: CustomerOutput
custWithdrawnOfferCustFS = CustomerOutput
    { coState       = Last $ Just Finished 
    , coThreadToken = Last $ Just threadToken
    , coParams      = Last $ Just params'
    }

------ customer withdrawn an offer - negative (withdrawing before deadline)  --------

custWithdrawnOfferNegT :: Trace.EmulatorTrace ()
custWithdrawnOfferNegT = do
    customerHdl <- Trace.activateContractWallet w1 customerContract'
    void $ Trace.waitNSlots 2
    Trace.callEndpoint @"withdraw-offer" customerHdl ()
    void $ Trace.waitNSlots 1

custWithdrawnOfferNegCustFS :: CustomerOutput
custWithdrawnOfferNegCustFS = CustomerOutput
    { coState       = Last $ Just $ OfferOpened jobDoneDeadline 
    , coThreadToken = Last $ Just threadToken
    , coParams      = Last $ Just params'
    }    

------ service provider accepts an offer - happy path  --------

servProvAcceptsOfferT :: Trace.EmulatorTrace ()
servProvAcceptsOfferT = do
    customerHdl <- Trace.activateContractWallet w1 customerContract'
    void $ Trace.waitNSlots 1
    token <- extractAssetClass customerHdl
    params <- extractParams customerHdl
    serviceProviderHdl <- Trace.activateContractWallet w2 (serviceProviderContract token params) 
    void $ Trace.waitNSlots 1
    Trace.callEndpoint @"accept-offer" serviceProviderHdl ()
    void $ Trace.waitNSlots 2


servProvAcceptsOfferCustFS :: CustomerOutput
servProvAcceptsOfferCustFS = CustomerOutput
    { coState       = Last $ Just $ OfferAccepted jobDoneDeadline serviceProvider
    , coThreadToken = Last $ Just threadToken
    , coParams      = Last $ Just params'
    } 

servProvAcceptsOfferServProvFS :: ServiceProviderOutput
servProvAcceptsOfferServProvFS = ServiceProviderOutput $ Last $ Just  $ OfferAccepted jobDoneDeadline serviceProvider 


------ service provider accepts an offer - negative (accepting after deadline)  --------

servProvAcceptsOfferNegT :: Trace.EmulatorTrace ()
servProvAcceptsOfferNegT = do
    customerHdl <- Trace.activateContractWallet w1 customerContract'
    void $ Trace.waitNSlots 1
    token <- extractAssetClass customerHdl
    params <- extractParams customerHdl
    serviceProviderHdl <- Trace.activateContractWallet w2 (serviceProviderContract token params) 
    void $ Trace.waitUntilTime (sapOfferAcceptanceDeadline params)
    void $ Trace.waitNSlots 10
    Trace.callEndpoint @"accept-offer" serviceProviderHdl ()
    void $ Trace.waitNSlots 2


servProvAcceptsOfferCustNegFS :: CustomerOutput
servProvAcceptsOfferCustNegFS = CustomerOutput
    { coState       = Last $ Just $ Finished
    , coThreadToken = Last $ Just threadToken
    , coParams      = Last $ Just params'
    } 

servProvAcceptsOfferServProvNegFS :: ServiceProviderOutput
servProvAcceptsOfferServProvNegFS = ServiceProviderOutput $ Last $ Just  Finished


------ customer claim job not done on time - happy path --------

custClaimsJobNotDoneT :: Trace.EmulatorTrace ()
custClaimsJobNotDoneT = do
    customerHdl <- Trace.activateContractWallet w1 customerContract'
    void $ Trace.waitNSlots 1
    token <- extractAssetClass customerHdl
    params <- extractParams customerHdl
    serviceProviderHdl <- Trace.activateContractWallet w2 (serviceProviderContract token params) 
    void $ Trace.waitNSlots 1
    Trace.callEndpoint @"accept-offer" serviceProviderHdl ()
    void $ Trace.waitUntilTime jobDoneDeadline
    void $ Trace.waitNSlots 2

custClaimsJobNotDoneCustFS :: CustomerOutput
custClaimsJobNotDoneCustFS = CustomerOutput
    { coState       = Last $ Just Finished
    , coThreadToken = Last $ Just threadToken
    , coParams      = Last $ Just params'
    }    

custClaimsJobNotDoneServProvFS :: ServiceProviderOutput
custClaimsJobNotDoneServProvFS = ServiceProviderOutput $ Last $ Just Finished

------ customer claim job not done on time - negative (claiming before deadline) --------

custClaimsJobNotDoneNegT :: Trace.EmulatorTrace ()
custClaimsJobNotDoneNegT = do
    customerHdl <- Trace.activateContractWallet w1 customerContract'
    void $ Trace.waitNSlots 1
    token <- extractAssetClass customerHdl
    params <- extractParams customerHdl
    serviceProviderHdl <- Trace.activateContractWallet w2 (serviceProviderContract token params) 
    void $ Trace.waitNSlots 1
    Trace.callEndpoint @"accept-offer" serviceProviderHdl ()
    void $ Trace.waitNSlots 5
    Trace.callEndpoint @"claim-job-not-done" customerHdl ()
    void $ Trace.waitNSlots 2


custClaimsJobNotDoneCustNegFS :: CustomerOutput
custClaimsJobNotDoneCustNegFS = CustomerOutput
    { coState       = Last $ Just (OfferAccepted jobDoneDeadline serviceProvider)
    , coThreadToken = Last $ Just threadToken
    , coParams      = Last $ Just params'
    }    

custClaimsJobNotDoneServProvNegFS :: ServiceProviderOutput
custClaimsJobNotDoneServProvNegFS = ServiceProviderOutput $ Last $ Just (OfferAccepted jobDoneDeadline serviceProvider)

------ service provider does the job on time - happy path --------

servProvJobOnTimeT :: Trace.EmulatorTrace ()
servProvJobOnTimeT = do
    customerHdl <- Trace.activateContractWallet w1 customerContract'
    void $ Trace.waitNSlots 1
    token <- extractAssetClass customerHdl
    params <- extractParams customerHdl
    serviceProviderHdl <- Trace.activateContractWallet w2 (serviceProviderContract token params) 
    void $ Trace.waitNSlots 1
    Trace.callEndpoint @"accept-offer" serviceProviderHdl ()
    void $ Trace.waitUntilTime (jobDoneDeadline - POSIXTime 10_000)
    Trace.callEndpoint @"claim-job-done" serviceProviderHdl jobDoneProof
    void $ Trace.waitNSlots 3

servProvJobOnTimeCustFS :: CustomerOutput
servProvJobOnTimeCustFS = CustomerOutput
    { coState       = Last $ Just Finished
    , coThreadToken = Last $ Just threadToken
    , coParams      = Last $ Just params'
    }    

servProvJobOnTimeServProvFS :: ServiceProviderOutput
servProvJobOnTimeServProvFS = ServiceProviderOutput $ Last $ Just Finished


------ service provider does the job on time - negative (proof invalid) --------

servProvJobOnTimeNegT :: Trace.EmulatorTrace ()
servProvJobOnTimeNegT = do
    customerHdl <- Trace.activateContractWallet w1 customerContract'
    void $ Trace.waitNSlots 1
    token <- extractAssetClass customerHdl
    params <- extractParams customerHdl
    serviceProviderHdl <- Trace.activateContractWallet w2 (serviceProviderContract token params) 
    void $ Trace.waitNSlots 1
    Trace.callEndpoint @"accept-offer" serviceProviderHdl ()
    void $ Trace.waitNSlots 10
    Trace.callEndpoint @"claim-job-done" serviceProviderHdl "invalid_proof"
    void $ Trace.waitNSlots 3

servProvJobOnTimeCustNegFS :: CustomerOutput
servProvJobOnTimeCustNegFS = CustomerOutput
    { coState       = Last $ Just $ OfferAccepted jobDoneDeadline serviceProvider
    , coThreadToken = Last $ Just threadToken
    , coParams      = Last $ Just params'
    }    


------ customer extends jobDoneDeadline --------

custExtendsJobDoneDeadlineT :: Trace.EmulatorTrace ()
custExtendsJobDoneDeadlineT = do
    customerHdl <- Trace.activateContractWallet w1 customerContract'
    void $ Trace.waitNSlots 1
    token <- extractAssetClass customerHdl
    params <- extractParams customerHdl
    serviceProviderHdl <- Trace.activateContractWallet w2 (serviceProviderContract token params)
    void $ Trace.waitNSlots 1
    Trace.callEndpoint @"accept-offer" serviceProviderHdl ()
    void $ Trace.waitNSlots 10
    Trace.callEndpoint @"extend-job-done-deadline" customerHdl jobDoneDeadlineExtended
    void $ Trace.waitNSlots 2

custExtendsJobDoneDeadlineTCustFS :: CustomerOutput
custExtendsJobDoneDeadlineTCustFS = CustomerOutput
    { coState       = Last $ Just $ OfferAccepted jobDoneDeadlineExtended serviceProvider
    , coThreadToken = Last $ Just threadToken
    , coParams      = Last $ Just params'
    }    

custExtendsJobDoneDeadlineServProvFS :: ServiceProviderOutput
custExtendsJobDoneDeadlineServProvFS = ServiceProviderOutput $ Last $ Just $ OfferAccepted jobDoneDeadlineExtended serviceProvider



------ service provider claims job done after customer extends jobDoneDeadline  --------

servProvClaimsJobDoneAfterCustomerExtendsJobDoneDeadlineT :: Trace.EmulatorTrace ()
servProvClaimsJobDoneAfterCustomerExtendsJobDoneDeadlineT = do
    customerHdl <- Trace.activateContractWallet w1 customerContract'
    void $ Trace.waitNSlots 1
    token <- extractAssetClass customerHdl
    params <- extractParams customerHdl
    serviceProviderHdl <- Trace.activateContractWallet w2 (serviceProviderContract token params)
    void $ Trace.waitNSlots 1
    Trace.callEndpoint @"accept-offer" serviceProviderHdl ()
    void $ Trace.waitNSlots 10
    Trace.callEndpoint @"extend-job-done-deadline" customerHdl jobDoneDeadlineExtended
    void $ Trace.waitUntilTime (jobDoneDeadlineExtended - POSIXTime 10)
    Trace.callEndpoint @"claim-job-done" serviceProviderHdl jobDoneProof
    void $ Trace.waitNSlots 3

servProvClaimsJobDoneAfterCustomerExtendsJobDoneDeadlineCustFS :: CustomerOutput
servProvClaimsJobDoneAfterCustomerExtendsJobDoneDeadlineCustFS = CustomerOutput
    { coState       = Last $ Just Finished
    , coThreadToken = Last $ Just threadToken
    , coParams      = Last $ Just params'
    }    

servProvClaimsJobDoneAfterCustomerExtendsJobDoneDeadlineServProvFS :: ServiceProviderOutput
servProvClaimsJobDoneAfterCustomerExtendsJobDoneDeadlineServProvFS = ServiceProviderOutput $ Last $ Just Finished


------ service provider opens a dispute --------

servProvOpensADisputeT :: Trace.EmulatorTrace ()
servProvOpensADisputeT = do
    customerHdl <- Trace.activateContractWallet w1 customerContract'
    void $ Trace.waitNSlots 1
    token <- extractAssetClass customerHdl
    params <- extractParams customerHdl
    serviceProviderHdl <- Trace.activateContractWallet w2 (serviceProviderContract token params)
    void $ Trace.waitNSlots 1
    Trace.callEndpoint @"accept-offer" serviceProviderHdl ()
    void $ Trace.waitUntilTime (jobDoneDeadline - POSIXTime 20_000)
    Trace.callEndpoint @"open-dispute" serviceProviderHdl ()
    void $ Trace.waitNSlots 3

servProvOpensADisputeCustFS :: CustomerOutput
servProvOpensADisputeCustFS = CustomerOutput
    { coState       = Last $ Just $ InDispute serviceProvider
    , coThreadToken = Last $ Just threadToken
    , coParams      = Last $ Just params'
    }    

servProvOpensADisputeServProvFS :: ServiceProviderOutput
servProvOpensADisputeServProvFS = ServiceProviderOutput $ Last $ Just $ InDispute serviceProvider


------ customer claims job not done - negative (dispute opened) --------

custClaimsJobDoneButDisputeOpenedNegT :: Trace.EmulatorTrace ()
custClaimsJobDoneButDisputeOpenedNegT = do
    customerHdl <- Trace.activateContractWallet w1 customerContract'
    void $ Trace.waitNSlots 1
    token <- extractAssetClass customerHdl
    params <- extractParams customerHdl
    serviceProviderHdl <- Trace.activateContractWallet w2 (serviceProviderContract token params)
    void $ Trace.waitNSlots 1
    Trace.callEndpoint @"accept-offer" serviceProviderHdl ()
    void $ Trace.waitUntilTime (jobDoneDeadline - POSIXTime 20_000)
    Trace.callEndpoint @"open-dispute" serviceProviderHdl ()
    void $ Trace.waitUntilTime jobDoneDeadline
    void $ Trace.waitNSlots 2
    Trace.callEndpoint @"claim-job-not-done" customerHdl ()
    void $ Trace.waitNSlots 2


custClaimsJobDoneButDisputeOpenedNegCustFS :: CustomerOutput
custClaimsJobDoneButDisputeOpenedNegCustFS = CustomerOutput
    { coState       = Last $ Just $ InDispute serviceProvider
    , coThreadToken = Last $ Just threadToken
    , coParams      = Last $ Just params'
    }    

custClaimsJobDoneButDisputeOpenedNegServProvFS :: ServiceProviderOutput
custClaimsJobDoneButDisputeOpenedNegServProvFS = ServiceProviderOutput $ Last $ Just $ InDispute serviceProvider

------ dispute provider decides that customer wins the dispute --------

dispProvDecidesCustWinsT :: Trace.EmulatorTrace ()
dispProvDecidesCustWinsT = do
    customerHdl <- Trace.activateContractWallet w1 customerContract'
    void $ Trace.waitNSlots 1
    token <- extractAssetClass customerHdl
    params <- extractParams customerHdl
    serviceProviderHdl <- Trace.activateContractWallet w2 (serviceProviderContract token params) 
    disputeProviderHdl <- Trace.activateContractWallet w3 (disputeProviderContract token params)
    void $ Trace.waitNSlots 1
    Trace.callEndpoint @"accept-offer" serviceProviderHdl ()
    void $ Trace.waitUntilTime (jobDoneDeadline - POSIXTime 20_000)
    Trace.callEndpoint @"open-dispute" serviceProviderHdl ()
    void $ Trace.waitNSlots 30
    Trace.callEndpoint @"resolve-dispute" disputeProviderHdl (CustomerWins customer) 
    void $ Trace.waitNSlots 3

dispProvDecidesCustWinsCustFS :: CustomerOutput
dispProvDecidesCustWinsCustFS = CustomerOutput
    { coState       = Last $ Just Finished
    , coThreadToken = Last $ Just threadToken
    , coParams      = Last $ Just params'
    }    

dispProvDecidesCustWinsServProvFS :: ServiceProviderOutput
dispProvDecidesCustWinsServProvFS = ServiceProviderOutput $ Last $ Just Finished 


------ dispute provider decides that service provider wins the dispute --------

dispProvDecidesServProvWinsT :: Trace.EmulatorTrace ()
dispProvDecidesServProvWinsT = do
    customerHdl <- Trace.activateContractWallet w1 customerContract'
    void $ Trace.waitNSlots 1
    token <- extractAssetClass customerHdl
    params <- extractParams customerHdl
    serviceProviderHdl <- Trace.activateContractWallet w2 (serviceProviderContract token params)
    disputeProviderHdl <- Trace.activateContractWallet w3 (disputeProviderContract token params)
    void $ Trace.waitNSlots 1
    Trace.callEndpoint @"accept-offer" serviceProviderHdl ()
    void $ Trace.waitUntilTime (jobDoneDeadline - POSIXTime 20_000)
    Trace.callEndpoint @"open-dispute" serviceProviderHdl ()
    void $ Trace.waitNSlots 30
    Trace.callEndpoint @"resolve-dispute" disputeProviderHdl (ServiceProviderWins serviceProvider) 
    void $ Trace.waitNSlots 3

dispProvDecidesServProvWinsCustFS :: CustomerOutput
dispProvDecidesServProvWinsCustFS = CustomerOutput
    { coState       = Last $ Just Finished
    , coThreadToken = Last $ Just threadToken
    , coParams      = Last $ Just params'
    }    

dispProvDecidesServProvWinsServProvFS :: ServiceProviderOutput
dispProvDecidesServProvWinsServProvFS = ServiceProviderOutput $ Last $ Just Finished 


tests :: TestTree
tests = testGroup "service agreement"
    [ 
      checkPredicateOptions options "As a customer I want to open an offer"
        (assertNotDone customerContract' (Trace.walletInstanceTag w1) "customer should be not done"
        .&&. assertAccumState customerContract' (Trace.walletInstanceTag w1) ((==) custOpensAnOfferCustFS ) "customer state should be OK" 
        .&&. walletFundsChange w1 (Ada.adaValueOf (- price) <> Ada.adaValueOf(- disputeFee))
        .&&. valueAtAddress (Scripts.validatorAddress $ typedValidator (threadToken, params')) (valuePrice <> valueDisputeFee <> threadTokenToValue threadToken ==))
        custOpensAnOfferT
        
    , checkPredicateOptions options "As a customer I want to withdraw an offer previously opened - happy path"
        (assertDone customerContract' (Trace.walletInstanceTag w1) (const True) "customer should be done"
        .&&. assertAccumState customerContract' (Trace.walletInstanceTag w1) ((==) custWithdrawnOfferCustFS ) "customer final state should be OK" 
        .&&. walletFundsChange w1 mempty
        .&&. valueAtAddress (Scripts.validatorAddress $ typedValidator (threadToken, params')) (mempty ==))
        custWithdrawnOfferT
    
    , checkPredicateOptions options "As a customer I want to withdraw an offer previously opened - negative (withdrawing before deadline) "
        (assertNotDone customerContract' (Trace.walletInstanceTag w1) "customer should not be done"
        .&&. assertAccumState customerContract' (Trace.walletInstanceTag w1) ((==) custWithdrawnOfferNegCustFS ) "customer state should be OK" 
        .&&. walletFundsChange w1 (Ada.adaValueOf (- price) <> Ada.adaValueOf(- disputeFee))
        .&&. valueAtAddress (Scripts.validatorAddress $ typedValidator (threadToken, params')) (valuePrice <> valueDisputeFee <> threadTokenToValue threadToken ==))
        custWithdrawnOfferNegT

    , checkPredicateOptions options "As a service provider I want to accept an offer - happy path"
        (assertNotDone customerContract' (Trace.walletInstanceTag w1) "customer should not be done"
        .&&. assertNotDone (serviceProviderContract threadToken params') (Trace.walletInstanceTag w2) "service provider should not be done"
        .&&. assertAccumState customerContract' (Trace.walletInstanceTag w1) ((==) servProvAcceptsOfferCustFS ) "customer state should be OK" 
        .&&. assertAccumState (serviceProviderContract threadToken params') (Trace.walletInstanceTag w2) ((==) servProvAcceptsOfferServProvFS) "service provider state should be OK" 
        .&&. walletFundsChange w1 (Ada.adaValueOf (- price) <> Ada.adaValueOf(- disputeFee))
        .&&. walletFundsChange w2 (Ada.adaValueOf (- serviceProviderCollateral))
        .&&. valueAtAddress (Scripts.validatorAddress $ typedValidator (threadToken, params')) (valuePrice <> valueDisputeFee <> valueServiceProviderCollateral <> threadTokenToValue threadToken ==))
        servProvAcceptsOfferT 

    , checkPredicateOptions options "As a service provider I want to accept an offer - negative (accepting after deadline)"
        (assertDone customerContract' (Trace.walletInstanceTag w1) (const True) "customer should be done"
        .&&. assertDone (serviceProviderContract threadToken params') (Trace.walletInstanceTag w2) (const True) "service provider should be done"
        .&&. assertAccumState customerContract' (Trace.walletInstanceTag w1) ((==) servProvAcceptsOfferCustNegFS ) "customer state should be OK" 
        .&&. assertAccumState (serviceProviderContract threadToken params') (Trace.walletInstanceTag w2) ((==) servProvAcceptsOfferServProvNegFS) "service provider state should be OK" 
        .&&. walletFundsChange w1 (Ada.adaValueOf (- price) <> Ada.adaValueOf(- disputeFee))
        .&&. walletFundsChange w2 (Ada.adaValueOf (- serviceProviderCollateral))
        .&&. valueAtAddress (Scripts.validatorAddress $ typedValidator (threadToken, params')) (mempty ==))
        servProvAcceptsOfferNegT     

    , checkPredicateOptions options "As a customer I want to claim that job has not been done on time - happy path"
        (assertDone customerContract' (Trace.walletInstanceTag w1) (const True) "customer should be done"
        .&&. assertDone (serviceProviderContract threadToken params') (Trace.walletInstanceTag w2) (const True) "service provider should be done"
        .&&. assertAccumState customerContract' (Trace.walletInstanceTag w1) ((==) custClaimsJobNotDoneCustFS ) "customer final state should be OK" 
        .&&. assertAccumState (serviceProviderContract threadToken params') (Trace.walletInstanceTag w2) ((==) custClaimsJobNotDoneServProvFS ) "service provider final state should be OK" 
        .&&. walletFundsChange w1 valueServiceProviderCollateral
        .&&. walletFundsChange w2 (Ada.adaValueOf (- serviceProviderCollateral))
        .&&. valueAtAddress (Scripts.validatorAddress $ typedValidator (threadToken, params')) (mempty ==))
        custClaimsJobNotDoneT 

    , checkPredicateOptions options "As a customer I want to claim that job has been not done on time - negative (claiming before deadline)"
        (assertNotDone customerContract' (Trace.walletInstanceTag w1) "customer should not be done"
        .&&. assertNotDone (serviceProviderContract threadToken params') (Trace.walletInstanceTag w2) "service provider should not be done"
        .&&. assertAccumState customerContract' (Trace.walletInstanceTag w1) ((==) custClaimsJobNotDoneCustNegFS ) "customer state should be OK" 
        .&&. assertAccumState (serviceProviderContract threadToken params') (Trace.walletInstanceTag w2) ((==) custClaimsJobNotDoneServProvNegFS ) "service provider state should be OK" 
        .&&. walletFundsChange w1 (Ada.adaValueOf (- price) <> Ada.adaValueOf(- disputeFee))
        .&&. walletFundsChange w2 (Ada.adaValueOf (- serviceProviderCollateral))
        .&&. valueAtAddress (Scripts.validatorAddress $ typedValidator (threadToken, params')) (valuePrice <> valueDisputeFee <> valueServiceProviderCollateral <> threadTokenToValue threadToken ==))
        custClaimsJobNotDoneNegT

    , checkPredicateOptions options "As a service provider I want to claim that I've done the job on time - happy path"
        ( assertDone customerContract' (Trace.walletInstanceTag w1) (const True) "customer should be done"
        .&&. assertDone (serviceProviderContract threadToken params') (Trace.walletInstanceTag w2) (const True) "service provider should be done"
        .&&. assertAccumState customerContract' (Trace.walletInstanceTag w1) ((==) servProvJobOnTimeCustFS ) "customer final state should be OK" 
        .&&. assertAccumState (serviceProviderContract threadToken params') (Trace.walletInstanceTag w2) ((==) servProvJobOnTimeServProvFS ) "service provider final state should be OK" 
        .&&. walletFundsChange w1 (Ada.adaValueOf (- price))
        .&&. walletFundsChange w2 valuePrice
        .&&. valueAtAddress (Scripts.validatorAddress $ typedValidator (threadToken, params')) (mempty ==))
        servProvJobOnTimeT 

    , checkPredicateOptions options "As a service provider I want to claim that I've done the job on time - negative (proof invalid)"
        ( assertNotDone customerContract' (Trace.walletInstanceTag w1) "customer should not be done"
        .&&. assertAccumState customerContract' (Trace.walletInstanceTag w1) ((==) servProvJobOnTimeCustNegFS ) "customer final state should be OK" 
        .&&. walletFundsChange w1 (Ada.adaValueOf (- price) <> Ada.adaValueOf (- disputeFee))
        .&&. walletFundsChange w2 (Ada.adaValueOf (- serviceProviderCollateral))
        .&&. assertFailedTransaction (\_ _ _ ->  True) 
        .&&. valueAtAddress (Scripts.validatorAddress $ typedValidator (threadToken, params')) (valuePrice <> valueDisputeFee <> valueServiceProviderCollateral <> threadTokenToValue threadToken ==))
        servProvJobOnTimeNegT 

    , checkPredicateOptions options "As a customer I want to extend jobDoneDeadline after a service provider has accepted the offer"
        ( assertNotDone customerContract' (Trace.walletInstanceTag w1) "customer should not be done"
        .&&. assertNotDone (serviceProviderContract threadToken params') (Trace.walletInstanceTag w2) "service provider should not be done"
        .&&. assertAccumState customerContract' (Trace.walletInstanceTag w1) ((==) custExtendsJobDoneDeadlineTCustFS ) "customer state should be OK" 
        .&&. assertAccumState (serviceProviderContract threadToken params') (Trace.walletInstanceTag w2) ((==) custExtendsJobDoneDeadlineServProvFS ) "service provider state should be OK" 
        .&&. walletFundsChange w1 (Ada.adaValueOf (- price) <> Ada.adaValueOf (- disputeFee))
        .&&. walletFundsChange w2 (Ada.adaValueOf (- serviceProviderCollateral))
        .&&. valueAtAddress (Scripts.validatorAddress $ typedValidator (threadToken, params')) (valuePrice <> valueDisputeFee <> valueServiceProviderCollateral <> threadTokenToValue threadToken ==))
        custExtendsJobDoneDeadlineT

    , checkPredicateOptions options "As a service provider I want to claim job done after customer has extended jobDoneDeadline"
        ( assertDone customerContract' (Trace.walletInstanceTag w1) (const True) "customer should be done"
        .&&. assertDone (serviceProviderContract threadToken params') (Trace.walletInstanceTag w2) (const True) "service provider should be done"
        .&&. assertAccumState customerContract' (Trace.walletInstanceTag w1) ((==) servProvClaimsJobDoneAfterCustomerExtendsJobDoneDeadlineCustFS ) "customer final state should be OK" 
        .&&. assertAccumState (serviceProviderContract threadToken params') (Trace.walletInstanceTag w2) ((==) servProvClaimsJobDoneAfterCustomerExtendsJobDoneDeadlineServProvFS ) "service provider final state should be OK" 
        .&&. walletFundsChange w1 (Ada.adaValueOf (- price))
        .&&. walletFundsChange w2 valuePrice
        .&&. valueAtAddress (Scripts.validatorAddress $ typedValidator (threadToken, params')) (mempty ==))
        servProvClaimsJobDoneAfterCustomerExtendsJobDoneDeadlineT

    , checkPredicateOptions options "As a service provider I want to open a dispute"
        ( assertNotDone customerContract' (Trace.walletInstanceTag w1) "customer should not be done"
        .&&. assertNotDone (serviceProviderContract threadToken params') (Trace.walletInstanceTag w2) "service provider should not be done"
        .&&. assertAccumState customerContract' (Trace.walletInstanceTag w1) ((==) servProvOpensADisputeCustFS) "customer state should be OK" 
        .&&. assertAccumState (serviceProviderContract threadToken params') (Trace.walletInstanceTag w2) ((==) servProvOpensADisputeServProvFS ) "service provider final state should be OK" 
        .&&. walletFundsChange w1 (Ada.adaValueOf (- price) <> Ada.adaValueOf (- disputeFee)) 
        .&&. walletFundsChange w2 (Ada.adaValueOf (- serviceProviderCollateral) <> Ada.adaValueOf (- disputeFee))
        .&&. valueAtAddress (Scripts.validatorAddress $ typedValidator (threadToken, params')) (valuePrice <> valueDisputeFee <> valueDisputeFee <> valueServiceProviderCollateral <> threadTokenToValue threadToken ==))
        servProvOpensADisputeT    

    , checkPredicateOptions options "As a customer I want to claim that job has not been done on time - negative (dispute opened) "
        ( assertNotDone customerContract' (Trace.walletInstanceTag w1) "customer should not be done"
        .&&. assertNotDone (serviceProviderContract threadToken params') (Trace.walletInstanceTag w2) "service provider should not be done"
        .&&. assertAccumState customerContract' (Trace.walletInstanceTag w1) ((==) custClaimsJobDoneButDisputeOpenedNegCustFS) "customer state should be OK" 
        .&&. assertAccumState (serviceProviderContract threadToken params') (Trace.walletInstanceTag w2) ((==) custClaimsJobDoneButDisputeOpenedNegServProvFS ) "service provider final state should be OK" 
        .&&. walletFundsChange w1 (Ada.adaValueOf (- price) <> Ada.adaValueOf (- disputeFee)) 
        .&&. walletFundsChange w2 (Ada.adaValueOf (- serviceProviderCollateral) <> Ada.adaValueOf (- disputeFee))
        .&&. valueAtAddress (Scripts.validatorAddress $ typedValidator (threadToken, params')) (valuePrice <> valueDisputeFee <> valueDisputeFee <> valueServiceProviderCollateral <> threadTokenToValue threadToken ==))
        custClaimsJobDoneButDisputeOpenedNegT  

    , checkPredicateOptions options "As a dispute provider I decide that the customer wins the dispute"
        ( assertDone customerContract' (Trace.walletInstanceTag w1) (const True) "customer should be done"
        .&&. assertDone (serviceProviderContract threadToken params') (Trace.walletInstanceTag w2) (const True) "service provider should be done"
        .&&. assertDone (disputeProviderContract threadToken params') (Trace.walletInstanceTag w3) (const True) "dispute provider should be done"
        .&&. assertAccumState customerContract' (Trace.walletInstanceTag w1) ((==) dispProvDecidesCustWinsCustFS) "service provider final state should be OK" 
        .&&. assertAccumState (serviceProviderContract threadToken params') (Trace.walletInstanceTag w2) ((==) dispProvDecidesCustWinsServProvFS ) "customer final state should be OK" 
        .&&. walletFundsChange w1 valueServiceProviderCollateral
        .&&. walletFundsChange w2 (Ada.adaValueOf (- serviceProviderCollateral) <> Ada.adaValueOf (- disputeFee))
        .&&. walletFundsChange w3 valueDisputeFee
        .&&. valueAtAddress (Scripts.validatorAddress $ typedValidator (threadToken, params')) (mempty ==))
        dispProvDecidesCustWinsT 

    , checkPredicateOptions options "As a dispute provider I decide that the service provider wins the dispute"
        ( assertDone customerContract' (Trace.walletInstanceTag w1) (const True) "customer should be done"
        .&&. assertDone (serviceProviderContract threadToken params') (Trace.walletInstanceTag w2) (const True) "service provider should be done"
        .&&. assertDone (disputeProviderContract threadToken params') (Trace.walletInstanceTag w3) (const True) "dispute provider should be done"
        .&&. assertAccumState customerContract' (Trace.walletInstanceTag w1) ((==) dispProvDecidesServProvWinsCustFS) "customer final state should be OK" 
        .&&. assertAccumState (serviceProviderContract threadToken params') (Trace.walletInstanceTag w2) ((==) dispProvDecidesServProvWinsServProvFS ) "service provider final state should be OK" 
        .&&. walletFundsChange w1 (Ada.adaValueOf (- disputeFee) <> Ada.adaValueOf (- price))
        .&&. walletFundsChange w2 valuePrice
        .&&. walletFundsChange w3 valueDisputeFee
        .&&. valueAtAddress (Scripts.validatorAddress $ typedValidator (threadToken, params')) (mempty ==))
        dispProvDecidesServProvWinsT                    
    ]


--- Utils for repl demo ---

runMyTrace :: Trace.EmulatorTrace () -> IO ()
runMyTrace = Trace.runEmulatorTraceIO' def (options ^. emulatorConfig) 
