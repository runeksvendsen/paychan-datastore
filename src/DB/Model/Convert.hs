{-# LANGUAGE ScopedTypeVariables #-}
module DB.Model.Convert where

import DB.Model.Entity
import DB.Types
import Util

import qualified Network.Google.Datastore as DS
import           Data.Typeable


parsePathElem :: forall a. Identifier a => DS.PathElement -> Either String (Ident a)
parsePathElem a = iName >>= check >>= Right . Ident . Right
    where iNum  = a ^. DS.peId  -- TODO: Is this always present?
          iName = maybe (Left "Missing type identifier") Right $ a ^. DS.peName
          kindStr = cs $ show (typeOf (undefined :: a))
          check n = if n == kindStr then Right n else
                    Left $ "Type mismatch. Found " ++ cs n ++ " expected " ++ cs kindStr

toPathElem :: forall a. Typeable a => Ident a -> PathElement
toPathElem (Ident iId) =
    DS.pathElement &
        DS.peKind ?~ cs kindStr &
        either (peId ?~) (peName ?~) iId
  where
    kindStr = show (typeOf (undefined :: a))

data LookupRes a =
    LookupRes a EntityVersion
  | LookupErr String

-- |Check entity version.
checkCommResponse :: EntityVersion -> CommitResponse -> UpdateResult
checkCommResponse prevVer commResp =
    case commResp ^. crMutationResults of
        [r]       -> if getMRVersion r > prevVer then Updated else NotUpdated
        []        -> NotUpdated
        n@(_:_:_) -> throw . InternalError $
            "DB BUG? More than one item was updated by 'txCommitUpdate': " ++ show n
  where
    getMRVersion r = fromMaybe
        (throw . InternalError $ "MutationResult: empty version field")
        (r ^. mrVersion)
