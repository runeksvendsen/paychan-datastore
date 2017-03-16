{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass, GADTs, FlexibleContexts, DataKinds, RecordWildCards #-}
module Storage where

import           Control.Monad.IO.Class     (liftIO)
import           Test.QuickCheck            (Gen, sample', vectorOf, choose, generate)
import           System.IO                  (stderr)
import           System.Environment         (getArgs)
import           Data.Time                  (getCurrentTime)

import qualified ChanDB                     as DB
import qualified PaymentChannel.Test        as Pay
import qualified PromissoryNote.Test        as Note
import qualified Control.Monad              as M
import           Control.Monad
import qualified Data.Time.Clock            as Clock
import qualified Control.Concurrent.Async   as Async
--import qualified Control.Monad.Catch            as Catch


-- TODO: Move to bitcoin-payment-channel?
genTestData :: Word -> IO Pay.ChannelPairResult
genTestData numPayments = do
    amountList <- map fromIntegral <$> generate
        (vectorOf (fromIntegral numPayments+1) (choose (0, 100) :: Gen Integer))
    (arbPair,_) <- fmap head $ sample' $ Pay.mkChanPairInitAmount (head amountList)
    Pay.runChanPair arbPair (tail amountList)


defaultPayCount :: Word
defaultPayCount = 25

defaultThreadCount :: Int
defaultThreadCount = 1

main :: IO ()
main = do
    -- Command-line args
    args <- getArgs
    let numThreads = if not (null args) then read (head args) :: Int else defaultThreadCount
    let payCount = if length args > 1 then read (args !! 1) :: Word else defaultPayCount
    -- Env/conf
    dbConf <- DB.getHandle DB.Info
    -- Test data
    tstDataLst <- M.replicateM numThreads $ genTestData payCount
    -- Go!
    putStrLn . unlines $ [ ""
                         , "Thread count: " ++ show numThreads
                         , "Pay    count: " ++ show payCount ++ " (per thread)" ]
    numPayLst <- Async.forConcurrently tstDataLst $ \tstData ->
        datastoreTest dbConf tstData
    putStrLn $ "\n\nDone! Executed " ++ show (sum numPayLst) ++ " payments."

runDbRethrow  :: forall c m h. DB.ChanDB m h
              => h -> m c -> IO c
runDbRethrow cfg = DB.throwLeft <=< DB.runDB cfg

datastoreTest :: DB.DatastoreConf -> Pay.ChannelPairResult -> IO Int
datastoreTest cfg Pay.ChannelPairResult{..} = do
    let sampleRecvChan = Pay.recvChan resInitPair
        sampleKey = Pay.getSendPubKey sampleRecvChan
        paymentList = reverse $ init resPayList
    _ <- runDbRethrow cfg (DB.create sampleRecvChan :: DB.Datastore ())
    -- Safe lookup + update/rollback
    res <- M.forM paymentList $ \paym -> do
            _ <- runDbRethrow cfg $ DB.atomically DB.PayChanDB cfg
                (payChanTest sampleKey paym :: DB.DatastoreTx (Pay.BtcAmount, DB.RecvPayChan))
            runDbRethrow cfg $ DB.atomically DB.ClearingDB cfg
                (clearingTest sampleKey paym :: DB.DatastoreTx DB.StoredNote)
    return $ length res


payChanTest :: ( DB.MonadIO txM
               , DB.ChanDBTx txM dbM cfg
               ) =>
               Pay.SendPubKey
            -> Pay.SignedPayment
            -> txM (Pay.BtcAmount, DB.RecvPayChan)
payChanTest pk payment = do
    chan <- Pay.fromMaybe (error "404") <$> DB.getPayChan pk
    resE <- liftIO $ Pay.acceptPayment chan (Pay.toPaymentData payment)
    case resE of
        Right (v,s) -> DB.updatePayChan s >> return (v,s)
        Left e -> error $ "recvPayment error :( " ++ show e

clearingTest :: ( DB.MonadIO txM
                , DB.ChanDBTx txM dbM cfg
                ) =>
                Pay.SendPubKey
             -> Pay.SignedPayment
             -> txM DB.StoredNote
clearingTest pk payment = do
    (val,s)     <- payChanTest pk payment
    noteM       <- DB.getNewestNote pk
    (_,newNote) <- either error id <$> mkNewNote (val,s) noteM
    DB.insertUpdNotes (pk, newNote, DB.setMostRecentNote False <$> noteM)
    return newNote
  where
    mkNewNote (val,s) prevNoteM = do
        now     <- liftIO getCurrentTime
        newNote <- createNewNote val now payment prevNoteM
        return $ Right (s, newNote)


createNewNote :: DB.MonadIO m
              => Note.Amount
              -> Clock.UTCTime
              -> Pay.SignedPayment
              -> Maybe DB.StoredNote
              -> m DB.StoredNote
createNewNote val now payment prevNoteM = do
    newNote <- liftIO $ head <$> sample' (Note.arbNoteOfValueT now val)
    let noteFromPrev prevPN =
            either error id $ DB.mkCheckStoredNote newNote prevPN (Pay.toPaymentData payment)
    return $ maybe
        ( DB.mkGenesisNote newNote (Pay.toPaymentData payment) )
        noteFromPrev
        prevNoteM

