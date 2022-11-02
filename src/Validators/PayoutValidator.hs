{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}

module Validators.PayoutValidator where

import PlutusTx
import PlutusTx.Prelude
import qualified Ledger
import qualified Plutus.V2.Ledger.Api as V2LedgerApi
import qualified Plutus.Script.Utils.V2.Scripts as V2UtilsScripts

--3 Onchain code

{-# INLINEABLE payoutV #-}
payoutV :: BuiltinData -> BuiltinData -> BuiltinData -> ()
payoutV _ _ _ = error ()

--Boilerplate
payoutVCompiled :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
payoutVCompiled = $$(compile [|| payoutV ||])

payoutValidator :: V2UtilsScripts.Validator
payoutValidator = V2LedgerApi.mkValidatorScript payoutVCompiled

validatorHash :: Ledger.ValidatorHash
validatorHash = Ledger.validatorHash payoutValidator

address :: Ledger.Address
address = Ledger.scriptHashAddress validatorHash