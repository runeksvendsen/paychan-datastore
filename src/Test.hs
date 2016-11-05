{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, GADTs, FlexibleContexts, DataKinds, RecordWildCards #-}
module Test where

import qualified DB
import           Model.PayState
import           Types
import qualified Data.Bitcoin.PaymentChannel.Test as Pay

import           Network.Google as Google
import           Network.Google.Datastore
import qualified Control.Monad as M
import qualified Control.Exception as Except
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Catch    (MonadCatch, MonadThrow)
import           Control.Monad.Trans.Control (MonadBaseControl)
import qualified Data.ByteString as BS
import qualified Data.Time.Clock as Clock
import           Data.Maybe                    (fromMaybe)
import           Data.Proxy (Proxy)
import           Control.Lens
import           System.IO              (stderr)
import           Test.QuickCheck (Gen, sample', vectorOf, choose, generate)
import           Data.String.Conversions          (cs)

import qualified Network.HTTP.Conduit as HTTP


projectId = "cloudstore-test"
payCount = 25


main :: IO ()
main = do
    putStrLn $ "Using project: " ++ cs projectId
    -- Request setup
    logger <- (Google.envLogger .~) <$> Google.newLogger Google.Error stderr
    env    <- Google.newEnv <&> logger . Google.allow (cloudPlatformScope ! datastoreScope)
    -- Gen test data
    amountList <- map fromIntegral <$> generate (vectorOf payCount (choose (0, 100) :: Gen Integer))
    (arbPair,_) <- fmap head $ sample' $ Pay.mkChanPairInitAmount (head amountList)
    let (chanPair, payAmtLst) = Pay.runChanPair arbPair (tail amountList)
    -- Test
    testDB env chanPair

testDB env Pay.ChannelPairResult{..} = do
    let sampleRecvChan = Pay.recvChan resInitPair
        sampleKey = Pay.getSenderPubKey sampleRecvChan
    putStrLn $ "Creating state for channel: " ++ showJsonStr (Pay.getSenderPK sampleKey)
    runResourceT . runGoogle env $ DB.insertChan projectId sampleRecvChan
    -- Safe lookup + update/rollback
    res <- M.forM (reverse $ init resPayList)
            (runResourceT . runGoogle env . doPayment projectId sampleKey)
    putStrLn $ "Done! Executed " ++ show (length res) ++ " payments."
    checkResult resRecvChan (last res)

checkResult :: Pay.RecvPayChan -> Either Pay.PayChanError Pay.RecvPayChan -> IO ()
checkResult testChan res = case res of
    (Right lastRpc) ->
        if lastRpc == testChan then
                putStrLn "Success! States match."
            else do
                putStrLn "State mismatch!"
                print lastRpc
                print testChan
    (Left e) ->
        error $ "Error in last result: " ++ show e

-- doPayment :: ( HasScope s '["https://www.googleapis.com/auth/cloud-platform"]
--              , MonadGoogle s m )
doPayment :: (MonadThrow m)
          => ProjectId
          -> Pay.SendPubKey
          -> Pay.FullPayment
          -> m (Either Pay.PayChanError Pay.RecvPayChan)
doPayment projectId key payment =
    DB.withDBState projectId key $ \recvChan -> do
        now <- Clock.getCurrentTime
        case Pay.recvPayment now recvChan payment of
            Left e -> do
                putStrLn $ "recvPayment error :( " ++ show e
                return $ Left e
            Right (a,s) -> do
                putStrLn $ "   #### Payment received: " ++ show a ++ "  ####"
                return $ Right s

