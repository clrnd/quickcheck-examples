{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Test.QuickCheck
import Data.Char (isUpper, isAlpha)
import qualified Data.Text as T

import Types
import Functions


prop_cleanfeed_capitalizes fs =
    all (all capital . T.splitOn " " . feedTitle) (cleanFeed fs)
  where
    capital w = T.null w ||
                let f =  head (T.unpack w) in
                    isUpper f || not (isAlpha f)

prop_cleanfeed_sorts (User _ _ fs) =
    let clean = cleanFeed fs
        dates = map feedDateTime clean
        pairs = zip dates (tail dates) in
    all (\(a, b) -> a < b) pairs

main :: IO ()
main = do
    quickCheck prop_cleanfeed_capitalizes
    quickCheck prop_cleanfeed_sorts
