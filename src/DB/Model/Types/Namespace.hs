{-# LANGUAGE RecordWildCards #-}
module DB.Model.Types.Namespace where

import qualified Network.Google.Datastore.Types as DS
import           Control.Lens
import qualified Data.Text as T
import           Text.Printf    (printf)

-- data NamespaceId = NamespaceId
--     { nsProjectId     :: T.Text
--     , nsNamespaceId   :: T.Text
--     }
--
-- toNamespaceId :: NamespaceId -> DS.NamespaceId
-- toNamespaceId NamespaceId{..} = DS.partitionId &
--     DS.piProjectId ?~ nsProjectId & -- TODO
--     DS.piNamespaceId ?~ nsNamespaceId
--
-- instance Show NamespaceId where
--     show (NamespaceId p n) =
--         printf "project:\"%s\"/namespace:\"%s\"" p n
