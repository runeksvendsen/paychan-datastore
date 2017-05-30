{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, DeriveAnyClass, GADTs, FlexibleContexts, DataKinds #-}
module ChanDB.Update
( txGetChanState
, txGetLastNote
)
where

import LibPrelude
import DB.Error
import ChanDB.Orphans ()
import ChanDB.Types
import DB.Query
import ChanDB.Types.StoredNote  (setMostRecentNote)
import DB.Tx.Safe
import DB.Request
import Control.Monad.Trans.Control
import Control.Monad.Base
import Control.Monad.Trans.Resource


type PayChanEnt = EntityWithAnc RecvPayChan Void
type NoteEnt    = EntityWithAnc StoredNote (Ident RecvPayChan)


txGetChanState :: ( DatastoreTxM m
                  , HasScope '[AuthDatastore] ProjectsRunQuery
                  )
               => Key
               -> m (Maybe RecvPayChan)
txGetChanState sendPK = do
    let key = root <//> sendPK :: KeyWithAnc RecvPayChan Void
    resL <- txLookup key
    return $ ancEntRes ( resL :: [(PayChanEnt, EntityVersion)] )


txGetLastNote :: ( DatastoreTxM m
                 , HasScope '[AuthDatastore] ProjectsRunQuery
                 )
              => Key
              -> m (Maybe StoredNote)
txGetLastNote k = do
    tx  <- getTxId
    ns  <- getNSId
    res <- entityQuery (Just ns) (Just tx) (qMostRecentNote k)
    return $ ancEntRes ( res :: [(NoteEnt, EntityVersion)] )

ancEntRes :: [(EntityWithAnc a k, EntityVersion)] -> Maybe a
ancEntRes = fmap (\(EntityWithAnc e _) -> e) . getFirstResult

qMostRecentNote :: Key
                -> AncestorQuery RecvPayChan (OfKind StoredNote (FilterProperty Bool Query))
qMostRecentNote k =
      AncestorQuery payChanId
    $ OfKind kind
    $ FilterProperty "most_recent_note" PFOEqual True
    query
  where
    kind = undefined :: StoredNote
    payChanId = getIdentifier k :: Ident RecvPayChan
