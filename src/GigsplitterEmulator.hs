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
    venueWallet           <- Emulator.activateContractWallet (Wallet.knownWallet 1) OffChain.endpoints
    managerWallet         <- Emulator.activateContractWallet (Wallet.knownWallet 2) OffChain.endpoints
    bassPlayerWallet      <- Emulator.activateContractWallet (Wallet.knownWallet 3) OffChain.endpoints
    drummerWallet         <- Emulator.activateContractWallet (Wallet.knownWallet 4) OffChain.endpoints
    guitarPlayerWallet    <- Emulator.activateContractWallet (Wallet.knownWallet 5) OffChain.endpoints
    singerWallet          <- Emulator.activateContractWallet (Wallet.knownWallet 6) OffChain.endpoints
    
    Emulator.callEndpoint @"Deposit" venueWallet $ OffChain.DepositParams {
          OffChain.venuePkh             = Wallet.mockWalletPaymentPubKeyHash $ Wallet.knownWallet 1,
          OffChain.managerPkh           = Wallet.mockWalletPaymentPubKeyHash $ Wallet.knownWallet 2,
          OffChain.bassPlayerPkh        = Wallet.mockWalletPaymentPubKeyHash $ Wallet.knownWallet 3, 
          OffChain.drummerPkh           = Wallet.mockWalletPaymentPubKeyHash $ Wallet.knownWallet 4,
          OffChain.guitarPlayerPkh      = Wallet.mockWalletPaymentPubKeyHash $ Wallet.knownWallet 5,
          OffChain.singerPkh            = Wallet.mockWalletPaymentPubKeyHash $ Wallet.knownWallet 6,
          OffChain.showTime             = TimeSlot.slotToBeginPOSIXTime def 1,
          OffChain.amountDeposited      = 20000000, -- 20 ADA (just for testing)
          OffChain.showId               = 1 -- key for specific Gig, to use in another onlne db
    }
    Extras.logInfo $ "Deposit Made"
    s <- waitNSlots 10
    Extras.logInfo $ "reached " ++ show s

    Emulator.callEndpoint @"Payout" managerWallet $ OffChain.PayoutParams {
          OffChain.ppVenuePkh               = Wallet.mockWalletPaymentPubKeyHash $ Wallet.knownWallet 1,
      --     OffChain.ppManagerPkh             = Wallet.mockWalletPaymentPubKeyHash $ Wallet.knownWallet 2,
          OffChain.ppBassPlayerPkh          = Wallet.mockWalletPaymentPubKeyHash $ Wallet.knownWallet 3, 
          OffChain.ppDrummerPkh             = Wallet.mockWalletPaymentPubKeyHash $ Wallet.knownWallet 4,
          OffChain.ppGuitarPlayerPkh        = Wallet.mockWalletPaymentPubKeyHash $ Wallet.knownWallet 5,
          OffChain.ppSingerPkh              = Wallet.mockWalletPaymentPubKeyHash $ Wallet.knownWallet 6,
          OffChain.ppShowTime               = TimeSlot.slotToBeginPOSIXTime def 1,
          OffChain.ppAmountDeposited        = 20000000,
          OffChain.ppShowId                 = 1
    }
    void $ waitNSlots 15
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
