module Vectorize.Tokenizer (
  tokenizeDoc
  , readStopWordsText
) where

import DataTypes.TfIdfTypes (Document(..),TfData(..),IdfData(..),Term,TfIdfEnv(..))
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import qualified Data.Text.Encoding.Base64 as TBase64

import Data.Maybe
import Data.Char

tokenizeDoc ::  T.Text -> S.Set T.Text -> [Term]
tokenizeDoc txt stopWords = do
--  stopWords <- readStopWordsText ""
  let txtList = T.toLower <$> (T.words txt)
  let filteredTerms = filter (\x -> S.notMember x stopWords) txtList
  filteredTerms

--splitOnAnyOf :: [T.Text] -> [T.Text] -> [[T.Text]]
----splitOnAnyOf :: Eq a => [[a]] -> [a] -> [[a]]
--splitOnAnyOf ds xs = T.foldl' (\ys d -> ys >>= T.splitOn d) [xs] ds

readStopWordsText :: FilePath -> IO (S.Set T.Text)
readStopWordsText stopFile =  do
  ls <- if (all isSpace stopFile) then
          fmap T.lines (TIO.readFile "./tfdata/corpora/stopwords/english")
        else fmap T.lines (TIO.readFile stopFile)
  let stopWordList = (foldl (\acc x -> (T.toLower x) : acc)[] ls)
  return (S.fromList stopWordList)
