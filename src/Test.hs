{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass, GADTs, FlexibleContexts, DataKinds, RecordWildCards #-}
module Test where

import           Util
import qualified ChanDB as DB
import           DB.Types
import qualified Data.Bitcoin.PaymentChannel.Test as Pay
import qualified PromissoryNote.Test as Note
import qualified PromissoryNote as Note

import qualified Control.Monad as M
import qualified Data.Time.Clock as Clock
import           System.IO                  (stderr, stdout, hFlush)
import           Test.QuickCheck            (Gen, arbitrary, sample', vectorOf, choose, generate)

import qualified Network.HTTP.Conduit as HTTP
import           Network.Google as Google
import qualified Control.Concurrent.Async as Async
import qualified Control.Monad.Logger as Log


projectId :: ProjectId
projectId = "cloudstore-test"

payCount :: Word
payCount = 100

threadCount :: Word
threadCount = 1

main :: IO ()
main = do
    let count = payCount
    let pid = projectId
    let numThreads = fromIntegral threadCount
    storeEnv   <- defaultAppDatastoreEnv
    tstDataLst <- M.replicateM numThreads $ genTestData count
    -- Go!
    putStrLn . unlines $ [ "Using project: " ++ cs pid
                         , "Thread count : " ++ show threadCount
                         , "Payment count: " ++ show count ++ " (per thread)" ]
    numPayLst <- Async.forConcurrently tstDataLst $ \tstData ->
        runPaymentTest pid storeEnv tstData
    putStrLn $ "\n\nDone! Executed " ++ show (sum numPayLst) ++ " payments."

runPaymentTest :: ProjectId -> Env '[AuthDatastore] -> Pay.ChannelPairResult -> IO Int
runPaymentTest pid env tstData = runResourceT . runGoogle env $ testDB pid tstData

testDB :: ( MonadCatch m
          , MonadGoogle '[AuthDatastore] m )
       => ProjectId -> Pay.ChannelPairResult -> m Int
testDB pid Pay.ChannelPairResult{..} = do
    let sampleRecvChan = Pay.recvChan resInitPair
        sampleKey = Pay.getSenderPubKey sampleRecvChan
        paymentList = reverse $ init resPayList
    _ <- DB.insertChan pid sampleRecvChan
    -- Safe lookup + update/rollback
    res <- M.forM paymentList (doPayment pid sampleKey)
--     _ <- DB.removeChan pid sampleKey
    return $ length res


defaultAppDatastoreEnv :: IO (Env '[AuthDatastore])
defaultAppDatastoreEnv = do
    manager <- HTTP.newManager HTTP.tlsManagerSettings
    logger <- Google.newLogger Google.Error stderr
    Google.newEnv <&>
        (envLogger .~ logger) .
        (envScopes .~ datastoreScope) .
        (envManager .~ manager)

genTestData :: Word -> IO Pay.ChannelPairResult
genTestData numPayments = do
    amountList <- map fromIntegral <$> generate
        (vectorOf (fromIntegral numPayments+1) (choose (0, 100) :: Gen Integer))
    (arbPair,_) <- fmap head $ sample' $ Pay.mkChanPairInitAmount (head amountList)
    let (chanPair, _) = Pay.runChanPair arbPair (tail amountList)
    return chanPair

doPayment :: ( MonadGoogle '[AuthDatastore] m )
          => ProjectId
          -> Pay.SendPubKey
          -> Pay.FullPayment
          -> m (Either DB.UpdateErr RecvPayChan)
doPayment pid key payment =
    DB.withDBStateNote pid key $ \recvChan noteM -> do
        now <- liftIO Clock.getCurrentTime
        case Pay.recvPayment now recvChan payment of
            Right (a,s) -> mkNewNote (a,s) (maybe Note.zeroUUID Note.getID noteM)
            Left e -> error ("recvPayment error :( " ++ show e) >> return (Left e)
  where
    mkNewNote (a,s) prevUUID = do
          newNote <- liftIO $ head <$> sample' (Note.arbNoteOfValue a)
          let newStoredNote = Note.mkStoredNote newNote prevUUID (Pay.channelValueLeft s)
          return $ Right (s,show newStoredNote `trace` newStoredNote)


--     mkNewNote :: ( MonadGoogle '[AuthDatastore] m, Log.MonadLogger m )
--               => (Pay.BitcoinAmount, Pay.RecvPayChanX)
--               -> Note.UUID
--               -> m (Either PayChanError (Pay.RecvPayChanX, StoredNote))