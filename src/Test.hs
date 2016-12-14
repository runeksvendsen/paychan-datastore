{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass, GADTs, FlexibleContexts, DataKinds, RecordWildCards #-}
module Test where

import           Util
import qualified ChanDB as DB
import           ChanDB.Types
import           DB.Util.Error
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


namespaceId :: NamespaceId
namespaceId = NamespaceId "cloudstore-test" "clearing"

payCount :: Word
payCount = 50

threadCount :: Word
threadCount = 2

main :: IO ()
main = do
    let count = payCount
    let ns = namespaceId
    let numThreads = fromIntegral threadCount
    storeEnv   <- defaultAppDatastoreEnv
    tstDataLst <- M.replicateM numThreads $ genTestData count
    -- Go!
    putStrLn . unlines $ [ ""
                         , "Partition ID: " ++ cs (show ns)
                         , "Thread count: " ++ show threadCount
                         , "Pay    count: " ++ show count ++ " (per thread)" ]
    numPayLst <- Async.forConcurrently tstDataLst $ \tstData ->
        runPaymentTest ns storeEnv tstData
    putStrLn $ "\n\nDone! Executed " ++ show (sum numPayLst) ++ " payments."

runPaymentTest :: NamespaceId -> Env '[AuthDatastore] -> Pay.ChannelPairResult -> IO Int
runPaymentTest ns env tstData = runResourceT . runGoogle env $ testDB ns tstData

testDB :: ( MonadCatch m
          , MonadGoogle '[AuthDatastore] m )
       => NamespaceId -> Pay.ChannelPairResult -> m Int
testDB ns Pay.ChannelPairResult{..} = do
    let sampleRecvChan = Pay.recvChan resInitPair
        sampleKey = Pay.getSenderPubKey sampleRecvChan
        paymentList = reverse $ init resPayList
    _ <- DB.insertChan ns sampleRecvChan
    -- Safe lookup + update/rollback
    res <- M.forM paymentList (doPayment ns sampleKey)
--     _ <- DB.removeChan ns sampleKey
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
          => NamespaceId
          -> Pay.SendPubKey
          -> Pay.FullPayment
          -> m (Either DB.UpdateErr RecvPayChan)
doPayment ns key payment =
    DB.withDBStateNote ns key $ \recvChan noteM -> do
        now <- liftIO Clock.getCurrentTime
        case Pay.recvPayment now recvChan payment of
            Right (a,s) -> mkNewNote (a,s) now noteM
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
    let noteFromPrev prevPN = either internalError id $
            Note.mkCheckStoredNote newNote prevPN (Pay.fpPayment payment)
    return $ maybe
        ( Note.mkGenesisNote newNote (Pay.fpPayment payment) )
        noteFromPrev
        prevNoteM

