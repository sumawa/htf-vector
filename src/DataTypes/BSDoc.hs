--{-# LANGUAGE DeriveGeneric, OverloadedStrings, ScopedTypeVariables #-}
--module BSDoc (
--  BSDoc(..)
--  , WordData1(..)
--) where
--
--import GHC.Generics
--import Data.Text
--
--import qualified Data.ByteString.Lazy.Char8 as BS
--
--import Data.Map
--
--data WordData1 = WordData1 Double Double Double deriving (Show,Eq)
--
--data BSDoc = BSDoc{
--    bagOfWords :: Map BS.ByteString WordData1
--    , wordCount :: Int
--    , vectorLength :: Double
--} deriving (Show,Generic)