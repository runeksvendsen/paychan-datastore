{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, DeriveAnyClass, GADTs, FlexibleContexts, DataKinds #-}
module DB.Request.Util where

import           DB.Types
import           DB.Util.Error


getFirstResult :: Either String [ ((a, Ident anc), EntityVersion) ] -> Maybe a
getFirstResult resE =
    either internalError id $
    fmap getFirst resE
  where
    getFirst res = case res of
        ( ((chan,_),_) : _ ) -> Just chan
        []                   -> Nothing
