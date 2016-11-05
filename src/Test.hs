{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass, GADTs, FlexibleContexts, DataKinds, RecordWildCards #-}
module Test where

import           Util
import qualified DB
import qualified Data.Bitcoin.PaymentChannel.Test as Pay

import qualified Control.Monad as M
import qualified Data.Time.Clock as Clock
import           System.IO                  (stderr)
import           Test.QuickCheck            (Gen, sample', vectorOf, choose, generate)

import qualified Network.HTTP.Conduit as HTTP
import           Network.Google as Google


projectId :: ProjectId
projectId = "cloudstore-test"

payCount :: Word
payCount = 25


main :: IO ()
main = do
    let count = payCount
    putStrLn . unlines $ [ "Starting channel test.",
                           "Executing " ++ show count ++ " payments.." ]
    numPayRes <- runPaymentTest projectId count
    putStrLn $ "Done! Executed " ++ show numPayRes ++ " payments."

runPaymentTest :: ProjectId -> Word -> IO Int
runPaymentTest pid numPayments = do
    putStrLn $ "Using project: " ++ cs pid
    storeEnv <- defaultAppDatastoreEnv
    tstData  <- genTestData numPayments
    -- Run
    runResourceT . runGoogle (storeEnv :: Env '[AuthDatastore]) $
        testDB pid tstData

defaultAppDatastoreEnv :: IO (Env '[AuthDatastore])
defaultAppDatastoreEnv = do
    manager <- HTTP.newManager HTTP.tlsManagerSettings
    logger <- Google.newLogger Google.Error stderr
    Google.newEnv <&>
        (envLogger .~ logger) .
        (envScopes .~ datastoreScope) .
        (envManager .~ manager)

testDB :: ( MonadCatch m
          , MonadGoogle '[AuthDatastore] m
          ,    HasScope '[AuthDatastore] BeginTransactionResponse
          ,    HasScope '[AuthDatastore] LookupResponse
          ,    HasScope '[AuthDatastore] RollbackResponse
          ,    HasScope '[AuthDatastore] CommitResponse )
       => ProjectId -> Pay.ChannelPairResult -> m Int
testDB pid Pay.ChannelPairResult{..} = do
    let sampleRecvChan = Pay.recvChan resInitPair
        sampleKey = Pay.getSenderPubKey sampleRecvChan
        paymentList = reverse $ init resPayList
    DB.insertChan pid sampleRecvChan
    -- Safe lookup + update/rollback
    res <- M.forM paymentList (doPayment pid sampleKey)
    return $ length res

genTestData numPayments = do
    amountList <- map fromIntegral <$> generate
        (vectorOf (fromIntegral numPayments+1) (choose (0, 100) :: Gen Integer))
    (arbPair,_) <- fmap head $ sample' $ Pay.mkChanPairInitAmount (head amountList)
    let (chanPair, _) = Pay.runChanPair arbPair (tail amountList)
    return chanPair

doPayment :: ( MonadCatch m
             , MonadGoogle '[AuthDatastore] m
             ,    HasScope '[AuthDatastore] BeginTransactionResponse
             ,    HasScope '[AuthDatastore] LookupResponse
             ,    HasScope '[AuthDatastore] RollbackResponse
             ,    HasScope '[AuthDatastore] CommitResponse )
          => ProjectId
          -> Pay.SendPubKey
          -> Pay.FullPayment
          -> m (Either Pay.PayChanError Pay.RecvPayChan)
doPayment pid key payment =
    DB.withDBState pid key $ \recvChan -> do
        now <- Clock.getCurrentTime
        case Pay.recvPayment now recvChan payment of
            Left e -> do
                putStrLn $ "recvPayment error :( " ++ show e
                return $ Left e
            Right (a,s) -> do
                putStrLn $ "   #### Payment received: " ++ show a ++ "  ####"
                return $ Right s

