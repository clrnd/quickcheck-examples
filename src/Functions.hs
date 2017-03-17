module Functions where

import Data.List (sortOn)
import qualified Data.Text as T

import Types


cleanFeed :: [Feed] -> [Feed]
cleanFeed  = map upperCase . sortOn feedDateTime
  where
    upperCase feed = let feedTitle' = T.toTitle (feedTitle feed) in
                     feed { feedTitle = feedTitle' }
