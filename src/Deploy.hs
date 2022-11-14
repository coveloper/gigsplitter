{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Deploy where
 
import           Cardano.Api       
import           Cardano.Api.Shelley                 (PlutusScript (..))
import           Codec.Serialise                    (serialise)
import qualified Data.Aeson                          as DataAeson
import qualified Data.ByteString.Lazy                as LBS
import qualified Data.ByteString.Short               as SBS
import qualified Plutus.V2.Ledger.Api                as LedgerApiV2
import qualified PlutusTx
--import qualified Ledger
import qualified Ledger                                          (PaymentPubKeyHash, unPaymentPubKeyHash)

import qualified GigsplitterOnChain                as OnChain

deploy :: IO()
deploy = do
    writeInitDatum
    --writeDepositDatum
    --_ <- writeMyFirstValidatorScript



    return ()

dataToScriptData :: LedgerApiV2.Data -> ScriptData
dataToScriptData (LedgerApiV2.Constr n xs) = ScriptDataConstructor n $ dataToScriptData <$> xs
dataToScriptData (LedgerApiV2.Map xs)      = ScriptDataMap [(dataToScriptData x, dataToScriptData y) | (x, y) <- xs]
dataToScriptData (LedgerApiV2.List xs)     = ScriptDataList $ dataToScriptData <$> xs
dataToScriptData (LedgerApiV2.I n)         = ScriptDataNumber n
dataToScriptData (LedgerApiV2.B bs)        = ScriptDataBytes bs


writeJSON :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeJSON file = LBS.writeFile file . DataAeson.encode . scriptDataToJson ScriptDataJsonDetailedSchema . dataToScriptData . PlutusTx.toData

writeValidator :: FilePath -> LedgerApiV2.Validator -> IO (Either (FileError ()) ())
writeValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV2) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . LedgerApiV2.unValidatorScript

writeInitDatum :: IO ()
writeInitDatum = writeJSON "../scripts/plutus-scripts/unit.json" ()

-- writeDepositDatum :: IO ()
-- writeDepositDatum = 
--     let contributor = OnChain.Dat
--             {
--                 OnChain.ddata = 89554122355
--             }
--         d = PlutusTx.toBuiltinData contributor
--     in writeJSON "../plutus-scripts/parameterized-datum.json" d

-- writeMyFirstValidatorScript :: IO (Either (FileError ()) ())
-- writeMyFirstValidatorScript = writeValidator "Deploy/Parameterized.plutus" $ OnChain.validator $ OnChain.BenParam 
--     {
--         OnChain.creator = Ledger.PaymentPubKeyHash "80b34df2162e9c4a38ce63322a8f903c9455a0bebd64c02cf1f3222a",
--         OnChain.beneficiary = Ledger.PaymentPubKeyHash "75eacb8808f937e42cde4312d2d4bb42bd1cbfca379bbe90a3ec0383",
--         OnChain.deadline = 1665105051000
--     }