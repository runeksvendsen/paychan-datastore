{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass, GADTs, FlexibleContexts, DataKinds, RecordWildCards #-}
{-# LANGUAGE UndecidableInstances, UndecidableSuperClasses, KindSignatures #-}
module Storage where

import           ChanDB as DB
-- Util
import           Control.Monad.IO.Class         (liftIO)
import           Control.Lens                   hiding (op)
import           Data.String.Conversions        (cs)


import qualified PaymentChannel.Test as Pay
import qualified PromissoryNote.Test as Note
import qualified PromissoryNote as Note

import qualified Control.Monad as M
import qualified Data.Time.Clock as Clock
import           System.IO                  (stderr)
import           Test.QuickCheck            (Gen, sample', vectorOf, choose, generate)

import qualified Network.HTTP.Conduit as HTTP
import           Network.Google as Google
import qualified Control.Concurrent.Async as Async
import           System.Environment (getArgs)

-- Query
import Data.Time                (getCurrentTime)



-- TODO: Move to bitcoin-payment-channel?
genTestData :: Word -> IO Pay.ChannelPairResult
genTestData numPayments = do
    amountList <- map fromIntegral <$> generate
        (vectorOf (fromIntegral numPayments+1) (choose (0, 100) :: Gen Integer))
    (arbPair,_) <- fmap head $ sample' $ Pay.mkChanPairInitAmount (head amountList)
    Pay.runChanPair arbPair (tail amountList)


projectId :: ProjectId
projectId = "cloudstore-test"

defaultPayCount :: Word
defaultPayCount = 25

defaultThreadCount :: Int
defaultThreadCount = 5

main :: IO ()
main = do
    -- Command-line args
    args <- getArgs
    let numThreads = if not (null args) then read (head args) :: Int else defaultThreadCount
    let payCount = if length args > 1 then read (args !! 1) :: Word else defaultPayCount
    -- Env/conf
    storeEnv <- DB.defaultAppDatastoreEnv
    let conf = DatastoreConf storeEnv projectId
    -- Test data
    tstDataLst <- M.replicateM numThreads $ genTestData payCount
    -- Go!
    putStrLn . unlines $ [ ""
                         , "Project ID:   " ++ cs projectId
                         , "Thread count: " ++ show numThreads
                         , "Pay    count: " ++ show payCount ++ " (per thread)" ]
    numPayLst <- Async.forConcurrently tstDataLst $ \tstData ->
        paymentTest conf tstData
    -- Query
--     DB.runDatastore conf queryTest
    putStrLn $ "\n\nDone! Executed " ++ show (sum numPayLst) ++ " payments."


-- runPaymentTest :: DatastoreConf -> Pay.ChannelPairResult -> IO Int
-- runPaymentTest cfg tstData = -- DB.runDatastore cfg $ paymentTest tstData



-- dbCreate :: (HasScope '[AuthDatastore] ProjectsRunQuery, DatastoreM m) => DatastoreConf -> m () -> IO ()
-- dbCreate cfg runThis = DB.runDB cfg $ runThis

paymentTest :: DatastoreConf -> Pay.ChannelPairResult -> IO Int
paymentTest cfg Pay.ChannelPairResult{..} = do
    let sampleRecvChan = Pay.recvChan resInitPair
        sampleKey = Pay.getSendPubKey sampleRecvChan
        paymentList = reverse $ init resPayList
    _ <- DB.runDatastore cfg (DB.create sampleRecvChan :: Datastore ())
    -- Safe lookup + update/rollback
    res <- M.forM paymentList $ \paym -> do
            atomically (PayChan cfg) $ doPayChan sampleKey paym
            atomically (Clearing cfg) $ doClearing sampleKey paym


--     _ <- DB.removeChan ns sampleKey
    return $ length res

-- queryTest :: Datastore ()
-- queryTest =
--     DB.selectChannels (CoveringValue 100000) >>= liftIO . print



doPayChan :: Pay.SendPubKey
          -> Pay.SignedPayment
          -> DatastoreTx (Pay.BtcAmount, RecvPayChan)
doPayChan pk payment = do
    chan <- Pay.fromMaybe (error "404") <$> DB.getPayChan pk
    resE <- liftIO $ Pay.acceptPayment chan (Pay.toPaymentData payment)
    case resE of
        Right (v,s) -> DB.updatePayChan s >> return (v,s)
        Left e -> error $ "recvPayment error :( " ++ show e

doClearing :: Pay.SendPubKey
          -> Pay.SignedPayment
          -> DatastoreTx StoredNote -- (Either DB.UpdateErr StoredNote)
doClearing pk payment = do
    (val,s) <- doPayChan pk payment

    noteM       <- getNewestNote pk
    (_,newNote) <- either error id <$> mkNewNote (val,s) noteM
    insertUpdNotes (pk, newNote, setMostRecentNote False <$> noteM)
    return newNote
  where
    mkNewNote (val,s) prevNoteM = do
        now     <- liftIO getCurrentTime
        newNote <- createNewNote val now payment prevNoteM
        return $ Right (s, newNote)


createNewNote :: MonadIO m
              => Note.Amount
              -> Clock.UTCTime
              -> Pay.SignedPayment
              -> Maybe StoredNote
              -> m StoredNote
createNewNote val now payment prevNoteM = do
    newNote <- liftIO $ head <$> sample' (Note.arbNoteOfValueT now val)
    let noteFromPrev prevPN =
            either error id $ DB.mkCheckStoredNote newNote prevPN (Pay.toPaymentData payment)
    return $ maybe
        ( DB.mkGenesisNote newNote (Pay.toPaymentData payment) )
        noteFromPrev
        prevNoteM

