{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeApplications #-}

module Trace where

import qualified Control.Foldl                 as L
import           Control.Monad.Freer           (run)
import           Control.Monad.Freer.Extras    as Extras
import           Data.Default                  (Default (..))
import           Data.Functor                  (void)
import           Data.Text
import           Data.Text.Lazy
import           Ledger
import           Ledger.TimeSlot
import           Plutus.Trace.Emulator         as Emulator
import           Plutus.Trace.Emulator.Types
import           Prelude
import qualified Streaming.Prelude             as S
import qualified Wallet.Emulator.Folds         as Folds
import           Wallet.Emulator.MultiAgent
import           Wallet.Emulator.Stream        (foldEmulatorStreamM)
import           Wallet.Emulator.Wallet

import           Plutus.Contract.Test          (w1, w2)


import qualified Data.Aeson
import           Data.Default

import           BlockfrostHello
import           Contract
import           Prettyprinter
import           Prettyprinter.Render.Terminal
import           Text.Pretty.Simple

test :: IO ()
test = runEmulatorTraceIO helloTrace

trace' :: IO ()
trace' = runEmulatorTraceIO' def def helloTrace
  where tconf = def {
    --showEvent = Just . show
    -- madly verbose with pretty-simple
--    showEvent = \evt -> case evt of
--        SchedulerEvent _ -> Nothing
--        x                -> Just $ Data.Text.unpack $ Data.Text.Lazy.toStrict $ pShow x
    showEvent = \e ->
        Data.Text.unpack
      . renderStrict
      . layoutPretty defaultLayoutOptions
      <$>
       myShowEvent e
    }

myShowEvent :: EmulatorEvent' -> Maybe (Doc AnsiStyle)
myShowEvent = \case
  UserThreadEvent (UserLog msg) -> Just . annotate (color Yellow) . pretty $ msg
  UserThreadEvent (UserThreadErr emsg) -> Just . annotate (color Red) $ "Emulator runtime error" <> pretty emsg
  InstanceEvent (ContractInstanceLog (ContractLog (Data.Aeson.String json)) _ _) -> Just . annotate (color Cyan) . pretty $ json
  InstanceEvent _ -> Nothing
  --InstanceEvent (ContractInstanceLog (StoppedWithError err) _ _) -> mempty --Just . showContractErrorLog $ err
  --InstanceEvent (ContractInstanceLog NoRequestsHandled _ _) -> mempty
  --InstanceEvent (ContractInstanceLog (HandledRequest _) _ _) -> mempty
  --InstanceEvent (ContractInstanceLog (CurrentRequests _) _ _) -> mempty
  --InstanceEvent (ContractInstanceLog (ReceiveEndpointCall desc json) _ _) -> mempty --Just . showContractRequestLog $ json
  SchedulerEvent _ -> Nothing
  ClientEvent _ _ -> Nothing
  ChainEvent _ -> Nothing
  ChainIndexEvent _ _ -> Nothing
  WalletEvent whichWallet evt -> Just . annotate (color Magenta) . pretty . show $ (whichWallet, evt)

helloStream = do
  let emulatorCfg = def
  let stream = runEmulatorStream emulatorCfg helloTrace
      getEvents :: Folds.EmulatorEventFold a -> a
      getEvents theFold = S.fst' $ run $ foldEmulatorStreamM (L.generalize theFold) stream

  return getEvents Folds.scriptEvents

helloTrace :: EmulatorTrace ()
helloTrace = do
    h1 <- activateContractWallet (w1) endpoints
    h2 <- activateContractWallet (w2) endpoints
    callEndpoint @"sayhello" h1 ()
    --callEndpoint @"grab" h1 grab2
    --void $ waitUntilSlot 20
--    callEndpoint @"saysomething" h2 "yolo!"
    s <- waitNSlots 1
    Extras.logInfo $ "reached " ++ show s

