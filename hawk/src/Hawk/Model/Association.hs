-- --------------------------------------------------------------------------
{- |
   Module      :  Hawk.Model.Association
   Copyright   :  Copyright (C) 2009 Björn Peemöller, Stefan Roggensack
   License     :  BSD3

   Maintainer  :  {inf6254, inf6509}fh-wedel.de
   Stability   :  experimental
   Portability :  portable
   Version     :  $Id: Main.hs 57 2009-05-29 11:33:59Z inf6254 $

-}
-- --------------------------------------------------------------------------

{-# LANGUAGE MultiParamTypeClasses, TypeFamilies #-}

module Hawk.Model.Association
    ( BelongsTo (..)
    , HasMany   (..)
    , HasOne    (..)
    ) where

import Hawk.Model.MonadDB
import Hawk.Model.WithForeignKey

class ForeignKeyRelationship r => BelongsTo r where
  getMaybeParent :: MonadDB m => r -> Referencing r -> m (Maybe (Referenced r))
  getParent      :: MonadDB m => r -> Referencing r -> m (Referenced r)
  setParent      :: MonadDB m => r -> Referencing r -> (Maybe (Referenced r)) -> m (Referencing r)

  getMaybeParent = getMaybeReferenced
  getParent      = getOneReferenced
  setParent      = setReferenced


class ForeignKeyRelationship r => HasMany r where
  getChildren :: MonadDB m => r -> Referenced r -> m [Referencing r]
  addChild    :: MonadDB m => r -> Referenced r -> Referencing r -> m (Referencing r)
  removeChild :: MonadDB m => r -> Referenced r -> Referencing r -> m (Referencing r)

  getChildren = getReferencings
  addChild    = addReferencing
  removeChild = removeReferencing

-- TODO Signatur festzurren!
class ForeignKeyRelationship r => HasOne r where
  getMaybeChild :: MonadDB m => r -> Referenced r -> m (Maybe (Referencing r))
  getChild      :: MonadDB m => r -> Referenced r -> m (Referencing r)
  replaceChild  :: MonadDB m => r -> Referenced r -> Maybe (Referencing r) -> m (Maybe (Referencing r),Maybe (Referencing r))

  getMaybeChild = getMaybeReferencing
  getChild      = getOneReferencing
  replaceChild  = replaceReferencing
