-- --------------------------------------------------------------------------
{- |
   Module      :  Hawk.Model.Model
   Copyright   :  Copyright (C) 2009 Björn Peemöller, Stefan Roggensack
   License     :  NONE

   Maintainer  :  {inf6254, inf6509}fh-wedel.de
   Stability   :  experimental
   Portability :  portable
   Version     :  $Id: Main.hs 57 2009-05-29 11:33:59Z inf6254 $

-}
-- --------------------------------------------------------------------------
module Hawk.Model.Model where

import Hawk.Model.Criteria (Criteria)
import Hawk.Model.MonadDB (MonadDB)
import Hawk.Model.Types (PrimaryKey)
import Hawk.Model.WithPrimaryKey
import Hawk.Model.Persistent

class WithPrimaryKey a => Model a where
  new              :: MonadDB m => m a
  count            :: MonadDB m => a -> Criteria -> m Integer
  select           :: MonadDB m => Criteria -> m [a]
  deleteByCriteria :: MonadDB m => a -> Criteria -> m Integer
  find             :: MonadDB m => [PrimaryKey] -> m [a]
  delete           :: MonadDB m => a -> m Bool
  insert           :: MonadDB m => a -> m a
  update           :: MonadDB m => a -> m a
  selectOne        :: MonadDB m => Criteria -> m a
  selectMaybe      :: MonadDB m => Criteria -> m (Maybe a)
  findOne          :: MonadDB m => PrimaryKey -> m a
  findMaybe        :: MonadDB m => PrimaryKey -> m (Maybe a)

  count            =  countPersistents
  select           =  selectPersistents
  deleteByCriteria =  deleteByCriteriaInTransaction
  find             =  findByPKs
  delete           =  deleteInTransaction
  insert           =  insertInTransaction
  update           =  updateInTransaction
  selectOne        =  selectOnePersistent
  selectMaybe      =  selectMaybePersistent
  findOne          =  findOneByPK
  findMaybe        =  findMaybeByPK

