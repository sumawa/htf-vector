module Vectorize.TfIdfVector(
  mkTermVectorTf
  , mkCorpus
  , mkTermVectorTfIdf
  , TfIdf(..)
  , CorpusTermsIdf(..)
  , getTfFromTermVector
) where

import DataTypes.TfIdfTypes (TermVector(..),TfData(..),IdfData(..),Term,DocTitle)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

import qualified Data.Text.Encoding.Base64 as TBase64

import Data.Maybe
import Data.Char

import Control.Lens
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class (lift)
import Data.Hashable

import Numeric

import Conduit
import Data.Conduit
--import qualified Data.Conduit.List as LC
import qualified Data.Text as T
import Data.Char (toUpper)

import Test.QuickCheck (Arbitrary(..))
import Data.Text.Arbitrary

data TfIdf = TfIdf {docMap :: M.Map T.Text TermVector
  , corpusDictionary :: M.Map T.Text IdfData
  , docCount :: Int} deriving (Show)

--type DocMap = M.Map T.Text TermVector
emptyDoc = TermVector {bagOfWords = M.empty, docWordCount = 0, vectorLength = 0}

-- Map of term and TfData alias as BagOfWords
type BagOfW = M.Map Term TfData
type BagOfWState = State BagOfW

type DocFreq = M.Map Term Double

-- increment count of term if already exist in BagOfW Map
incWordC :: Term -> BagOfWState BagOfW
incWordC t = do
  bow <- get
  let maybeElem = M.lookup t bow
  let tfData = incCount maybeElem
  let updatedBow = M.insert t tfData bow
  return updatedBow

incCount (Just (TfData count tf tfidf) ) = (TfData (count+1) tf tfidf)
incCount Nothing = TfData 1 0 0

--incTfDataCount (TfData )

-- TF Computations
mkTermVectorTf :: [Term] -> TermVector
mkTermVectorTf terms = doc where
  updatedBowWCount = foldl (\acc x -> evalState (incWordC x) acc) (M.empty) terms
  updatedBowWTf = evalState evaluateTfForDocS updatedBowWCount
  doc = TermVector {bagOfWords = updatedBowWTf, docWordCount = M.size updatedBowWTf, vectorLength = 0}

evaluateTfForDocS :: BagOfWState BagOfW
evaluateTfForDocS = do
  bow <- get
  let tokenCount = M.size bow
  let updatedBow = M.foldrWithKey (\k tfData res -> M.insert k (calcTf tfData tokenCount) res ) (M.empty) bow
  return updatedBow

calcTf :: TfData -> Int -> TfData
calcTf (TfData count tf tfidf) countTermsInDoc = (TfData count count tfidf)
-- other formula for tf computation
--calcTf (TfData count tf tfidf) countTermsInDoc = (TfData count (count/fromIntegral countTermsInDoc) tfidf)


-- DOC FREQ AND IDF COMPUTATIONS

newtype CorpusTermsIdf = CorpusTermsIdf {docTermFreq :: M.Map Term Double} deriving (Show)

-- | compute inverse document frequency of each term within the corpus.
--
-- The inverse document frequency is a measure of how much information the word provides,
--
-- Idf represents popularity of a "term" relative to the corpus.
mkCorpus ::  M.Map T.Text TermVector -> Int -> CorpusTermsIdf
mkCorpus termVectorsMap docCount =  evalState (corpusTokenState docCount) (allWordsInCorpus termVectorsMap)

allWordsInCorpus termVectorsMap = M.foldrWithKey (\t termVector acc -> (M.keys (bagOfWords termVector)) ++ acc) [] termVectorsMap

corpusTokenState :: Int -> State [T.Text] CorpusTermsIdf
corpusTokenState docCount = do
  corpusTokens <- get
  let docFreq = incCorpusTermCount corpusTokens
  let corpusDataWithIdf = updateIdfMap docFreq docCount
  return $ CorpusTermsIdf corpusDataWithIdf

incCorpusTermCount :: [T.Text] -> M.Map T.Text Double
incCorpusTermCount (x:xs) = foldl (\acc x -> M.insertWith (+) x 1 acc) M.empty (x:xs)

updateIdfMap :: M.Map T.Text Double -> Int -> M.Map T.Text Double
updateIdfMap mp docCount = M.foldrWithKey (\k v res -> M.insert k (calcIdfDouble v docCount) res ) (M.empty) mp

calcIdfDouble :: Double -> Int -> Double
calcIdfDouble termDocFreq corpusDocCount = abs $ logBase (2.718281828459) ( ( fromIntegral (corpusDocCount) +1) /termDocFreq+1 )

--  FINAL TFIDF COMPUTATION
-- | Compute tf X idf values
--
--  The final TfIdf value of the term within the document "within" the "corpus"
--
mkTermVectorTfIdf :: CorpusTermsIdf -> M.Map DocTitle TermVector -> M.Map DocTitle TermVector
mkTermVectorTfIdf t = evalState (termVectorTfIdfState t)

termVectorTfIdfState :: CorpusTermsIdf -> State (M.Map T.Text TermVector) (M.Map T.Text TermVector)
termVectorTfIdfState corpusTerms = do
  titleTermVectorMap <- get
  let corpusWords = (docTermFreq corpusTerms)
  let updatedDocMap = M.foldrWithKey (\k v res -> M.insert k (updateDocTfIdf v corpusWords) res ) (M.empty) titleTermVectorMap
  return updatedDocMap

updateDocTfIdf :: TermVector -> M.Map Term Double -> TermVector
updateDocTfIdf (TermVector bow dwCount vl) corpusWords = newTdoc where
  updatedBow = M.foldrWithKey (\k v res -> M.insert k (calcTfIdf v (M.lookup k corpusWords)) res ) (M.empty) bow
  newTdoc = TermVector updatedBow dwCount vl

calcTfIdf :: TfData -> Maybe Double -> TfData
calcTfIdf (TfData count tf tfidf) maybeDfData = case maybeDfData of
  (Just idf) -> (TfData count tf (count * idf) )
  Nothing -> (TfData count tf tfidf)

--calcIdf (IdfData count idf) corpusDocCount = IdfData count (logBase (10) ( ( fromIntegral (corpusDocCount) +1) /(count+1) ) )
--calcIdf (IdfData count idf) corpusDocCount = IdfData count (logBase (2) ( ( fromIntegral (corpusDocCount) +1) /(count+1) ) )

instance Arbitrary TermVector where
  arbitrary = mkTermVectorTf <$> arbitrary
  
getTfFromTermVector :: Term -> TermVector -> TfData
--getTf term tv = tfVal (fromMaybe (TfData 0.0 0.0 0.0 ) $ M.lookup term (bagOfWords tv))
getTfFromTermVector term tv = (fromMaybe (TfData 0.0 0.0 0.0 ) $ M.lookup term (bagOfWords tv))

--tfVal :: TfData -> Double
--tfVal (TfData tfc tf tfidf) = tf
--tf term tv = HM.lookupDefault 0 term (docTermFrequencies doc)  