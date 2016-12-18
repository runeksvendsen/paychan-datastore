{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass, GADTs, FlexibleContexts, DataKinds, RecordWildCards #-}
module Storage where

import           Util
import           ChanDB as DB

import qualified Data.Bitcoin.PaymentChannel.Test as Pay
import qualified PromissoryNote.Test as Note
import qualified PromissoryNote as Note

import qualified Control.Monad as M
import qualified Data.Time.Clock as Clock
import           System.IO                  (stderr)
import           Test.QuickCheck            (Gen, sample', vectorOf, choose, generate)

import qualified Network.HTTP.Conduit as HTTP
import           Network.Google as Google
import qualified Control.Concurrent.Async as Async

-- Query
import Data.Time                (getCurrentTime)
import Data.Time.Clock.POSIX    (posixSecondsToUTCTime)


projectId :: ProjectId
projectId = "cloudstore-test"

payCount :: Word
payCount = 25

threadCount :: Word
threadCount = 20

main :: IO ()
main = do
    let count = payCount
    let numThreads = fromIntegral threadCount
    storeEnv   <- defaultAppDatastoreEnv
    let conf = DatastoreConf storeEnv projectId
    tstDataLst <- M.replicateM numThreads $ genTestData count
    -- Go!
    putStrLn . unlines $ [ ""
                         , "Project ID:   " ++ cs projectId
                         , "Thread count: " ++ show threadCount
                         , "Pay    count: " ++ show count ++ " (per thread)" ]
    numPayLst <- Async.forConcurrently tstDataLst $ \tstData ->
        runPaymentTest conf tstData
    DB.runDatastore conf queryTest
    putStrLn $ "\n\nDone! Executed " ++ show (sum numPayLst) ++ " payments."


runPaymentTest :: DatastoreConf -> Pay.ChannelPairResult -> IO Int
runPaymentTest cfg tstData = DB.runDatastore cfg $ paymentTest tstData

paymentTest :: Pay.ChannelPairResult -> Datastore Int
paymentTest Pay.ChannelPairResult{..} = do
    let sampleRecvChan = Pay.recvChan resInitPair
        sampleKey = Pay.getSenderPubKey sampleRecvChan
        paymentList = reverse $ init resPayList
    _ <- DB.create sampleRecvChan
    -- Safe lookup + update/rollback
    res <- M.forM paymentList (doPayment sampleKey)
--     _ <- DB.removeChan ns sampleKey
    return $ length res

queryTest :: Datastore ()
queryTest = do
--     now <- liftIO $ getCurrentTime
    hey <- selectChannels $ ExpiringBefore (posixSecondsToUTCTime 2795556940)
    liftIO $ print hey
    liftIO $ putStrLn "##################### Notes ################"
    DB.selectNotes undefined >>= liftIO . print

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

doPayment :: HasScope '[AuthDatastore] ProjectsRunQuery
          => Pay.SendPubKey
          -> Pay.FullPayment
          -> Datastore (Either DB.UpdateErr RecvPayChan)
doPayment key payment = do
    _ <- DB.paychanWithState key $ \pChan -> do
            now <- liftIO Clock.getCurrentTime
            case Pay.recvPayment now pChan payment of
                Right (_,s) -> return $ Right s
                Left e -> error ("recvPayment error :( " ++ show e) >> return (Left e)
    DB.noteWithState key $ \pChan noteM -> do
        now <- liftIO Clock.getCurrentTime
        case Pay.recvPayment now pChan payment of
            Right (a,s) -> do
                r@(Right (_,note)) <- mkNewNote (a,s) now noteM
                return r

            Left e -> error ("recvPayment error :( " ++ show e) >> return (Left e)
  where
    mkNewNote (val,s) now prevNoteM = do
        newNote <- createNewNote val now payment prevNoteM
        return $ Right (s, newNote)


createNewNote :: MonadIO m
              => Note.Amount
              -> Clock.UTCTime
              -> Pay.FullPayment
              -> Maybe StoredNote
              -> m StoredNote
createNewNote val now payment prevNoteM = do
    newNote <- liftIO $ head <$> sample' (Note.arbNoteOfValue val now)
    let noteFromPrev prevPN = either error id $
            Note.mkCheckStoredNote newNote prevPN (Pay.fpPayment payment)
    return $ maybe
        ( Note.mkGenesisNote newNote (Pay.fpPayment payment) )
        noteFromPrev
        prevNoteM

