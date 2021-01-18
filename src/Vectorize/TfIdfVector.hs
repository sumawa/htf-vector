module Vectorize.TfIdfVector(
  mkTermVectorTf
  , mkCorpusIdf
  , mkTermVectorTfIdf
  , TfIdf(..)
) where

import DataTypes.TfIdfTypes (Document(..),TfData(..),IdfData(..),Term)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

import qualified Data.Text.Encoding.Base64 as TBase64

import Data.Maybe
import Data.Char

import Control.Lens
import Control.Monad.Trans.State
import Control.Monad.Trans.Class (lift)
import Data.Hashable

import Numeric

import Conduit
import Data.Conduit
--import qualified Data.Conduit.List as LC
import qualified Data.Text as T
import Data.Char (toUpper)


data TfIdf = TfIdf {docMap :: M.Map T.Text Document
  , corpusDictionary :: M.Map T.Text IdfData
  , docCount :: Int} deriving (Show)

--type DocMap = M.Map T.Text Document
emptyDoc = Document {bagOfWords = M.empty, docWordCount = 0, vectorLength = 0}

mkTermVectorTf :: [Term] -> Document
mkTermVectorTf terms = doc where
  docWithWordCounts = foldl (\acc x -> evalState (incWordCounts x) acc) (emptyDoc) terms
  docWithTfValues = evalState evaluateTfForDoc docWithWordCounts
  doc = docWithTfValues

incWordCounts :: T.Text -> State Document Document
incWordCounts t = do
  doc <- get
  let tokenCount = docWordCount doc
  let bow = (bagOfWords doc)
  let maybeElem = M.lookup t bow
  let tfData = incCount maybeElem
  let upBow = M.insert t tfData bow
  return (Document {bagOfWords = upBow, docWordCount = tokenCount, vectorLength = 0})

incCount (Just (TfData count tf tfidf) ) = (TfData (count+1) tf tfidf)
incCount Nothing = TfData 1 0 0

evaluateTfForDoc :: State Document Document
evaluateTfForDoc = do
  doc <- get
  let bow = (bagOfWords doc)
  let tokenCount = M.size bow
--  let upBow = M.foldrWithKey (\k (TfData count tf tfidf) res -> M.insert k (TfData count (count/fromIntegral tokenCount) tfidf) res ) (M.empty) bow
  let upBow = M.foldrWithKey (\k (TfData count tf tfidf) res -> M.insert k (TfData count count tfidf) res ) (M.empty) bow
  return (Document {bagOfWords = upBow, docWordCount = tokenCount, vectorLength = 0})

-- | compute inverse document frequency of each term within the corpus.
--
-- The inverse document frequency is a measure of how much information the word provides,
--
-- Idf represents popularity of a "term" relative to the corpus.
mkCorpusIdf :: TfIdf -> TfIdf
mkCorpusIdf t =  evalState corpusIdfState t

corpusIdfState :: State TfIdf TfIdf
corpusIdfState = do
  tfidf <- get
  let corpusWords = (corpusDictionary tfidf)
  let dm = (docMap tfidf)
  let totalDocCount = (docCount tfidf)
  let setOfTermsInEachDoc = M.foldrWithKey (\k (Document bagOfWords docWordCount vectorLength) acc -> (M.keysSet bagOfWords ) : acc ) [] dm
  let idfDataWithDocFreq = foldl (\acc keyset -> countDocFreq keyset acc) M.empty setOfTermsInEachDoc
  let idfDataWithIdf = updateCorpusWordsWithIdf idfDataWithDocFreq totalDocCount
  return (TfIdf {docMap = dm
            , corpusDictionary = idfDataWithIdf
            , docCount = totalDocCount})

countDocFreq :: S.Set Term -> M.Map Term IdfData -> M.Map Term IdfData
countDocFreq keys idfMap = foldl (\acc k -> M.insert k (incrementDfCount (M.lookup k idfMap) ) acc ) (idfMap) keys

incrementDfCount (Just (IdfData dfCount idf) ) = (IdfData (dfCount+1) idf)
incrementDfCount Nothing = IdfData 1 0

updateCorpusWordsWithIdf :: M.Map T.Text IdfData -> Int -> M.Map T.Text IdfData
updateCorpusWordsWithIdf mp docCount = M.foldrWithKey (\k v res -> M.insert k (calcIdf v docCount) res ) (M.empty) mp

calcIdf :: IdfData -> Int -> IdfData
calcIdf (IdfData count idf) corpusDocCount = IdfData count (logBase (2.718281828459) ( ( fromIntegral (corpusDocCount) +1) /(count+1) ) )
--calcIdf (IdfData count idf) corpusDocCount = IdfData count (logBase (10) ( ( fromIntegral (corpusDocCount) +1) /(count+1) ) )
--calcIdf (IdfData count idf) corpusDocCount = IdfData count (logBase (2) ( ( fromIntegral (corpusDocCount) +1) /(count+1) ) )

-- Compute TfIdf
-- | Compute tf X idf values
--
--  The final TfIdf value of the term within the document "within" the "corpus"
--
mkTermVectorTfIdf :: TfIdf -> TfIdf
mkTermVectorTfIdf t = evalState termVectorTfIdfState t

termVectorTfIdfState :: State TfIdf TfIdf
termVectorTfIdfState = do
  tfidf <- get
  let corpusWords = (corpusDictionary tfidf)
  let dm = (docMap tfidf)
  let totalDocCount = (docCount tfidf)
  let updatedDocMap = M.foldrWithKey (\k v res -> M.insert k (updateDocTfIdf v corpusWords) res ) (M.empty) dm
  return (TfIdf {docMap = updatedDocMap
            , corpusDictionary = corpusWords
            , docCount = totalDocCount})

updateDocTfIdf :: Document -> M.Map T.Text IdfData -> Document
updateDocTfIdf (Document bow dwCount vl) dfWordData = newTdoc where
  updatedBow = M.foldrWithKey (\k v res -> M.insert k (calcTfIdf v (M.lookup k dfWordData)) res ) (M.empty) bow
  newTdoc = Document updatedBow dwCount vl

calcTfIdf :: TfData -> Maybe IdfData -> TfData
calcTfIdf (TfData count tf tfidf) maybeDfData = case maybeDfData of
  (Just (IdfData dfCount idf)) -> (TfData count tf (count * idf) )
  Nothing -> (TfData count tf tfidf)
