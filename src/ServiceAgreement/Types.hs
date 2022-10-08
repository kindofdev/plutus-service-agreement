{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module ServiceAgreement.Types where 

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Ledger (POSIXTime(..), PaymentPubKeyHash, Value)
import Plutus.Contract.StateMachine (StateMachine (..))
import PlutusTx qualified
import PlutusTx.Prelude
import Prelude qualified as Haskell


type ServiceProvider = PaymentPubKeyHash
type Customer        = PaymentPubKeyHash
type DisputeProvider = PaymentPubKeyHash

type JobDoneDeadline         = POSIXTime
type OfferAcceptanceDeadline = POSIXTime

type JobDoneProof       = BuiltinByteString
type JobDoneVerificator = BuiltinByteString 
type JobDoneNonce       = BuiltinByteString

 
-- sha2_256 (JobDoneNonce `appendByteString` JobDoneProof) == JobDoneVerificator

-- | Definition of a service agreement.
data ServiceAgreementParams = ServiceAgreementParams
    { sapCustomer                  :: !Customer 
    , sapAgreement                 :: !BuiltinByteString
    , sapServiceProviderCollateral :: !Value
    , sapPrice                     :: !Value
    , sapDisputeFee                :: !Value
    , sapOfferAcceptanceDeadline   :: !OfferAcceptanceDeadline
    , sapJobDoneVerificator        :: !JobDoneVerificator
    , sapJobDoneNonce              :: !JobDoneNonce
    , sapDisputeProvider           :: !DisputeProvider
    }
    deriving stock (Generic, Haskell.Show, Haskell.Eq)
    deriving anyclass (ToJSON, FromJSON)

PlutusTx.makeLift ''ServiceAgreementParams

-- | The states of the service agreement.
data ServiceAgreementState
    = OfferOpened JobDoneDeadline 
    | OfferAccepted JobDoneDeadline ServiceProvider
    | InDispute ServiceProvider
    | Finished     -- TODOR Check with Roberto the case keeping info in the final state (ContractEnded is Nothing)
    deriving stock (Generic, Haskell.Show, Haskell.Eq)
    deriving anyclass (ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''ServiceAgreementState  

data DisputeResolution 
    = ServiceProviderWins ServiceProvider
    | CustomerWins Customer
    deriving stock (Generic, Haskell.Show, Haskell.Eq)
    deriving anyclass (ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''DisputeResolution  


-- | Transition for service agreement contract.
data ServiceAgreementInput
    = WithdrawOffer
    | AcceptOffer ServiceProvider 
    | ExtendJobDoneDeadline JobDoneDeadline
    | ClaimJobDone JobDoneProof
    | OpenDispute
    | ClaimJobNotDone
    | ResolveDispute DisputeResolution
    deriving stock (Generic, Haskell.Show)
    deriving anyclass (ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''ServiceAgreementInput

type ServiceAgreementMachine = StateMachine ServiceAgreementState ServiceAgreementInput

initialState :: JobDoneDeadline -> ServiceAgreementState
initialState = OfferOpened
