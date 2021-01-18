module Vectorize.Tokenizer (
  tokenizeDoc
) where

import DataTypes.TfIdfTypes (Document(..),TfData(..),IdfData(..),Term,TfIdfEnv(..))
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

import qualified Data.Text.Encoding.Base64 as TBase64

import Data.Maybe
import Data.Char

tokenizeDoc ::  T.Text -> TfIdfEnv -> [Term]
tokenizeDoc txt env = terms where
  txtList = T.toLower <$> (T.words txt)
  filteredStrList = filter (\x -> S.notMember x (stopWords env)) txtList
  terms = filteredStrList

--splitOnAnyOf :: [T.Text] -> [T.Text] -> [[T.Text]]
----splitOnAnyOf :: Eq a => [[a]] -> [a] -> [[a]]
--splitOnAnyOf ds xs = T.foldl' (\ys d -> ys >>= T.splitOn d) [xs] ds

