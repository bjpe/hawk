module App.Model.Customer.Functions where

import App.Model.Customer.Type
import App.Model.Customer.ForeignKeys
import App.Model.Category.Type (Category)
import Hawk.Model

getCategory :: MonadDB m => Customer -> m Category
getCategory = getParent Customer2Category