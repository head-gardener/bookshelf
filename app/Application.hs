{-# OPTIONS_GHC -Wno-orphans #-}

module Application where

import Foundation
import Yesod
import Routers

mkYesodDispatch "BookShelf" resourcesBookShelf
