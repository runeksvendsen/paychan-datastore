{-# LANGUAGE ScopedTypeVariables #-}
module DB.Model.Convert.Entity
(
  module DB.Model.Convert.Entity
, module DB.Model.Convert.Identifier
, module DB.Model.Types.Entity
)
where

import DB.Model.Types.Entity
import DB.Model.Convert.Identifier
import Types
import Util
import qualified Data.HashMap.Strict        as Map
import qualified Network.Google.Datastore   as DS
import           Data.Void                    (Void)


-- import Debug.Trace

encodeKey :: forall a anc. HasAncestor a anc => Ident anc -> Ident a -> Tagged a DS.Key
encodeKey anc a = identKey anc </> identKey a


-- encodeCompleteKey :: forall a anc. HasAncestor a anc => Ident anc -> Ident a -> Tagged a DS.Key
-- encodeCompleteKey anc a = Tagged $
--     unTagged (encodeKey anc :: Tagged anc DS.Key) <>
--         mkDSKey [ toPathElem a ]
--

parseKey :: forall a anc. HasAncestor a anc => Tagged a DS.Key -> Either String (Ident anc, Ident a)
parseKey a = case unTagged a ^. DS.kPath of
    [idn]       -> (,) (Ident $ Left 0) <$> parsePathElem (Tagged idn)
    [anc , idn] -> (,) <$> parsePathElem (Tagged anc) <*> parsePathElem (Tagged idn)
    path        -> Left $ "Unexpected PathElements: " ++ show path

encodeEntity :: forall a anc.
                HasAncestor a anc
             => Ident anc
             -> a
             -> Tagged a DS.Entity
encodeEntity anc a = Tagged $ DS.entity
    & DS.eKey ?~ unTagged (encodeKey anc (getIdent a))
    & DS.eProperties ?~ DS.entityProperties props
        where props = jsonToDS (excludeKeys a) <$> properties a

parseEntity :: forall a anc.
               HasAncestor a anc
            => Tagged a DS.Entity
            -> Either String (a, Ident anc)
parseEntity entT = do
    nKey <- getNativeKey entT
    (ancIdent, _) <- parseKey nKey
    a <- decodeProperties (Tagged $ props entT)
    return (a, ancIdent)
  where
    getNativeKey eT = Tagged <$> nativeKeyE eT :: Either String (Tagged a DS.Key)
    nativeKeyE eT = maybe (Left "No key in Entity") Right (unTagged eT ^. DS.eKey)
    -- Properties
    props eT = fromMaybe Map.empty $ ( ^. DS.epAddtional ) <$> unTagged eT ^. DS.eProperties



