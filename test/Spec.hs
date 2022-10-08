module Main
    ( main
    ) where

import qualified Spec.ServiceAgreement
import           Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "token sale" [ Spec.ServiceAgreement.tests ]
