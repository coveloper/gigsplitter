{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds        #-}

module GigsplitterEmulator where

--Plutus modules
import qualified Plutus.Trace.Emulator as Emulator
import Data.Default               (Default (..))
import Control.Monad.Freer.Extras as Extras
import Data.Functor               (void)
import Plutus.Trace
import qualified Wallet.Emulator.Wallet as Wallet
import qualified Ledger.TimeSlot as TimeSlot

-- Our offchain code
import qualified GigsplitterOffChain as OffChain


test :: IO ()
test = Emulator.runEmulatorTraceIO trace1


trace1 :: Emulator.EmulatorTrace ()
trace1 = do
    venueWallet     <- Emulator.activateContractWallet (Wallet.knownWallet 1) OffChain.endpoints
    managerWallet   <- Emulator.activateContractWallet (Wallet.knownWallet 2) OffChain.endpoints
    singerWallet    <- Emulator.activateContractWallet (Wallet.knownWallet 3) OffChain.endpoints
    bassWallet      <- Emulator.activateContractWallet (Wallet.knownWallet 4) OffChain.endpoints
    drumsWallet     <- Emulator.activateContractWallet (Wallet.knownWallet 5) OffChain.endpoints
    guitarWallet    <- Emulator.activateContractWallet (Wallet.knownWallet 6) OffChain.endpoints

    Emulator.callEndpoint @"Deposit" venueWallet $ OffChain.DepositParams {
          OffChain.venuePerson    = Wallet.mockWalletPaymentPubKeyHash $ Wallet.knownWallet 1,
          OffChain.managerPerson  = Wallet.mockWalletPaymentPubKeyHash $ Wallet.knownWallet 2, 
          OffChain.singerPerson   = Wallet.mockWalletPaymentPubKeyHash $ Wallet.knownWallet 3,
          OffChain.bassPerson     = Wallet.mockWalletPaymentPubKeyHash $ Wallet.knownWallet 4, 
          OffChain.drumsPerson    = Wallet.mockWalletPaymentPubKeyHash $ Wallet.knownWallet 5,
          OffChain.guitarPerson   = Wallet.mockWalletPaymentPubKeyHash $ Wallet.knownWallet 6,
          OffChain.paymentDeadline   = TimeSlot.slotToBeginPOSIXTime def 30,
        --   OffChain.amountDeposited   = 25000000, -- 25 ADA (makes the playout splits easier for now)
          OffChain.amountDeposited   = 1000000, -- 1 ADA (just for testing)
          OffChain.showId            = 1 -- key for specific Gig, to use in another onlne db
    }
    Extras.logInfo $ "Deposit Made"
    s <- waitNSlots 2
    Extras.logInfo $ "reached " ++ show s
    
    -- Emulator.callEndpoint @"Payout" venueWallet $ OffChain.DepositParams {
    --       OffChain.recipientVenue    = Wallet.mockWalletPaymentPubKeyHash $ Wallet.knownWallet 1,
    --       OffChain.recipientManager  = Wallet.mockWalletPaymentPubKeyHash $ Wallet.knownWallet 2, 
    --       OffChain.recipientSinger   = Wallet.mockWalletPaymentPubKeyHash $ Wallet.knownWallet 3,
    --       OffChain.recipientBass     = Wallet.mockWalletPaymentPubKeyHash $ Wallet.knownWallet 4, 
    --       OffChain.recipientDrums    = Wallet.mockWalletPaymentPubKeyHash $ Wallet.knownWallet 5,
    --       OffChain.recipientGuitar   = Wallet.mockWalletPaymentPubKeyHash $ Wallet.knownWallet 6,
    --       OffChain.paymentDeadline   = TimeSlot.slotToBeginPOSIXTime def 30,
    --       OffChain.amountDeposited   = 2500000000, -- 2500 ADA (makes the playout splits easier for now)
    --       OffChain.showId            = 1 -- key for specific Gig, to use in another onlne db
    -- }
    -- void $ waitNSlots 15
    -- Emulator.callEndpoint @"Grab" h1 $ OffChain.GrabParams {
    --       OffChain.gpCreator  = Wallet.mockWalletPaymentPubKeyHash $ Wallet.knownWallet 3
    --     , OffChain.gpDeadline = TimeSlot.slotToBeginPOSIXTime def 15
    --     , OffChain.gpGuess    = 1
    -- }
    -- void $ waitNSlots 15
    -- Emulator.callEndpoint @"Grab" h2 $ OffChain.GrabParams {
    --       OffChain.gpCreator  = Wallet.mockWalletPaymentPubKeyHash $ Wallet.knownWallet 3
    --     , OffChain.gpDeadline = TimeSlot.slotToBeginPOSIXTime def 30
    --     , OffChain.gpGuess    = 20
    -- }
    -- s <- waitNSlots 2
    -- Extras.logInfo $ "reached " ++ show s
    Extras.logInfo $ "reached "
