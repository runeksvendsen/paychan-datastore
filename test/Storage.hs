{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass, GADTs, FlexibleContexts, DataKinds, RecordWildCards #-}
module Storage where

import TestPrelude
import qualified ChanDB                     as DB
import qualified PaymentChannel.Test        as Pay
import qualified PromissoryNote.Test        as Note
import qualified Control.Concurrent.Async   as Async


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
    dbConf <- DB.getHandle stderr DB.LevelInfo
    -- Test data
    tstDataLst <- replicateM numThreads $ genTestData payCount
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
        sampleKey = Pay.getSecret sampleRecvChan
        paymentList = reverse $ init resPayList
    _ <- runDbRethrow cfg (DB.create sampleRecvChan :: DB.Datastore ())
    -- TMP
    DB.runDB cfg (DB.create sampleRecvChan :: DB.Datastore ()) >>= print
    -- Safe lookup + update/rollback
    res <- forM paymentList $ \paym -> do
            _ <- runDbRethrow cfg $ DB.liftDbTx DB.PayChanDB cfg
                (payChanTest sampleKey paym :: DB.DatastoreTx (Pay.BtcAmount, DB.RecvPayChan))
            runDbRethrow cfg $ DB.liftDbTx DB.ClearingDB cfg
                (clearingTest sampleKey paym :: DB.DatastoreTx DB.StoredNote)
    return $ length res


payChanTest :: ( DB.MonadIO txM
               , DB.ChanDBTx txM dbM cfg
               ) =>
               DB.Key
            -> Pay.SignedPayment
            -> txM (Pay.BtcAmount, DB.RecvPayChan)
payChanTest pk payment = do
    chan <- Pay.fromMaybe (error "404") <$> DB.getPayChan pk
    resE <- liftIO $ Pay.acceptPayment (Pay.toPaymentData payment) chan
    case resE of
        Right (s,v) -> DB.updatePayChan s >> return (v,s)
        Left e -> error $ "recvPayment error :( " ++ show e

clearingTest :: ( DB.MonadIO txM
                , DB.ChanDBTx txM dbM cfg
                ) =>
                DB.Key
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
              -> UTCTime
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

