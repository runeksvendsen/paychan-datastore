{-# LANGUAGE DeriveGeneric, DeriveAnyClass, RecordWildCards #-}
module ChanDB.Types.StoredNote
(
  StoredNote
, mkGenesisNote, mkCheckStoredNote
, setMostRecentNote
, unwrapStoredNote
)
where

import LibPrelude
import PromissoryNote.Types
import qualified RBPCP.Types as RBPCP
import ChanDB.Orphans ()

import           GHC.Generics
import qualified Data.Serialize     as Bin
import           Data.Aeson           (FromJSON, ToJSON)
import qualified Data.Aeson         as JSON
import Datastore                        (Identifier(..), HasProperties(..))


data StoredNote = StoredNote
    { promissory_note       :: PromissoryNote       -- ^ The actual note
    , previous_note_id      :: UUID                 -- ^ ID of the note issued before this one
    , payment_source        :: RBPCP.PaymentData    -- ^ Payment against which this note is issued
    , most_recent_note      :: Bool                 -- ^ Is this the newest note (chain tip)?
    } deriving (Show, Generic, ToJSON, FromJSON, Bin.Serialize)

unwrapStoredNote :: StoredNote -> PromissoryNote
unwrapStoredNote = promissory_note

instance HasUUID StoredNote where
    serializeForID StoredNote{..} = serializeForID promissory_note

instance Identifier StoredNote where
    objectId = objectId . getUUID

instance HasProperties StoredNote where
    encodeProps = (\(JSON.Object o) -> o) . JSON.toJSON
    decodeProps = decodeJsonObject

mkGenesisNote :: PromissoryNote -> RBPCP.PaymentData -> StoredNote
mkGenesisNote pn p = StoredNote pn zeroUUID p True

-- | Create note for storage given previous note,
-- also double check note/previous+current payment value
mkCheckStoredNote :: PromissoryNote -> StoredNote -> RBPCP.PaymentData -> Either String StoredNote
mkCheckStoredNote newPN prevSN@(StoredNote _ _ storedPmnt _) newPmnt =
    checkPaymentValue >>=
        \pn -> Right $ StoredNote pn (getUUID prevSN) newPmnt True
  where
    checkPaymentValue =
        if face_value (base_note newPN) == storedPmnt `diff` newPmnt then
            Right newPN
        else
            Left $ "BUG: mkCheckStoredNote: Value mismatch.\n" ++
                unlines [ show $ face_value (base_note newPN)
                        , show $ storedPmnt `diff` newPmnt
                        , show newPN
                        , show prevSN
                        , show newPmnt
                        ]

diff :: RBPCP.PaymentData -> RBPCP.PaymentData -> BtcAmount
diff p1 p2 =
    fromIntegral (RBPCP.paymentDataChangeValue p1) -
        fromIntegral (RBPCP.paymentDataChangeValue p2)

setMostRecentNote :: Bool -> StoredNote -> StoredNote
setMostRecentNote b n = n { most_recent_note = b }

