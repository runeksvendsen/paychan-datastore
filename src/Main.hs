{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, GADTs, FlexibleContexts, DataKinds, RecordWildCards #-}
module Main where

import           Types
import           DB.Mutate
import           Model.PayState
import qualified Data.Bitcoin.PaymentChannel.Test as Pay

import           Network.Google as Google
import           Network.Google.Datastore
import qualified Control.Monad as M
import qualified Control.Exception as Except
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Catch    (MonadCatch)
import qualified Data.ByteString as BS
import qualified Data.Time.Clock as Clock
import           Data.Maybe                    (fromMaybe)
import           Data.Proxy (Proxy)
import           Control.Lens
import           System.IO              (stderr)
import           Test.QuickCheck (Gen, sample', vectorOf, choose, generate)
import           Data.String.Conversions          (cs)

import qualified Network.HTTP.Conduit as HTTP




data ChannelOp =
    CreateChan  Pay.RecvPayChan
  | DeleteChan  Pay.SendPubKey
  | BeginPay    Pay.SendPubKey          -- Tx begin, lookup
  | FinishPay   Pay.RecvPayChan TxId    -- Tx commit
  | AbortPay    TxId                    -- Tx rollback




data UpdateResult = Updated | NotUpdated deriving Show


projectId = "cloudstore-test"
payCount = 25

main :: IO ()
main = do
    putStrLn $ "Using project: " ++ cs projectId
    -- Request setup
    man    <- HTTP.newManager HTTP.tlsManagerSettings
    cred   <- getApplicationDefault man
    logger <- Google.newLogger Google.Error stderr
    env    <- Google.newEnv <&> (Google.envLogger .~ logger) . Google.allow cloudPlatformScope
--                 Google.allow
    -- Gen
    amountList <- map fromIntegral <$> generate (vectorOf payCount (choose (0, 100) :: Gen Integer))
    print amountList
    (arbPair,_) <- fmap head $ sample' $ Pay.mkChanPairInitAmount (head amountList)
    let (Pay.ChannelPairResult{..}, payAmtLst) = Pay.runChanPair arbPair (tail amountList)
    -- TestDB
    let sampleRecvChan = Pay.recvChan resInitPair
    let sampleKey = Pay.getSenderPubKey sampleRecvChan
    putStrLn $ "Creating state for channel: " ++ showJsonStr (Pay.getSenderPK sampleKey)
    createChan env sampleRecvChan
    -- Lookup + safe update or rollback
    res <- M.forM (reverse $ init resPayList) (doPayment env sampleKey)
    putStrLn $ "Done! Executed " ++ show (length res) ++
        " payments. payLst length: " ++ show (length payAmtLst)
    case last res of
        (Right lastRpc,_) ->
            if lastRpc == resRecvChan then
                    putStrLn "Success! States match."
                else do
                    print lastRpc
                    print resRecvChan
                    error "State mismatch!"
        (Left e, _) ->
            error $ "Error in last result: " ++ show e
    print (map snd res)

doPayment env key payment = withDBState env key $ \recvChan -> do
    now <- Clock.getCurrentTime
    case Pay.recvPayment now recvChan payment of
        Left e -> do
            putStrLn $ "recvPayment error :( " ++ show e
            return $ Left e
        Right (a,s) -> do
            putStrLn $ "   #### Payment received: " ++ show a ++ "  ####"
            return $ Right s


