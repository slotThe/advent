module Prelude
    ( module Exports
    , HashMap
    , Text
    , fromList
    , toList
    , tshow
    ) where

import BasePrelude as Exports hiding (toList)

import qualified Data.Text as T

import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import GHC.Exts (fromList, toList)

tshow :: Show a => a -> Text
tshow = T.pack . show
