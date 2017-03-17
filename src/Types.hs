{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Types where

import Data.Char (chr)
import Data.String (fromString)
import Data.Text (Text)
import Data.UTC
import Test.QuickCheck


data User = User {
    userName :: Text
  , userAge :: Int
  , userFeed :: [Feed]
  } deriving (Show, Eq)

data Feed = Feed {
    feedTitle :: Text
  , feedDateTime :: DateTime
  } deriving (Show, Eq)

instance Arbitrary DateTime where
    arbitrary = do
        let maxDays = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
        y <- choose (1970, 2017)
        m <- choose (1, 12)
        d <- choose (1, maxDays !! fromIntegral (m - 1))
        h <- choose (0, 23)
        m' <- choose (0, 59)
        s <- choose (0, 59)

        let e = epoch
        let Just t = setSecond s e >>= setMinute m' >>= setHour h
                     >>= setDay d >>= setMonth m >>= setYear y
        return t


instance Arbitrary User where
    arbitrary = do
        userName <- fromString <$> arbitrary
        userAge <- arbitrary
        userFeed <- arbitrary
        return User {..}

instance Arbitrary Feed where
    arbitrary = do
        feedTitle <- fromString <$> words
        feedDateTime <- arbitrary
        return Feed {..}
      where
        words = listOf (chr <$> choose (35, 126))
