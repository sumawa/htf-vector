{-# LANGUAGE FlexibleContexts, DeriveGeneric, OverloadedStrings #-}

module Vectorize.StreamingVectorAPI where

import Vectorize.TfIdfVector (TfIdf(..), mkTermVectorTf, mkCorpus, mkTermVectorTfIdf,CorpusTermsIdf)
import Vectorize.Tokenizer (tokenizeDoc,readStopWordsText)
import DataTypes.TfIdfTypes (TermVector(..),TfData(..), Term,TfIdfEnv(..),DocTitle)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Char
import Data.Typeable
import System.Directory (listDirectory)
--import Data.ByteString.Lazy.Char8 as C -- from utf8-string
import Data.Word

import qualified Data.List as L
import qualified Data.Set as Set
import Control.Monad.IO.Class
import Conduit (ConduitT)
import Conduit

import Control.Monad (forM, forM_, mapM_, mapM)
import qualified Data.Conduit.List as L

import Data.ByteString    (ByteString)
import Data.String.Conversions (cs)

import GHC.Generics

-- | Tokenize a set of "documents" (files) generating a
-- list of pairs (DocTitle -> List of Term) where DocTitle will be the file name
--
-- Example:
--
--
tokenize :: String -> IO [(DocTitle,[Term])]
tokenize inputDir = do
  stopWords <- readStopWordsText ""
  txtArr <- loadData inputDir
  let docTokens = (fmap (\(k,t) -> (k,tokenizeDoc t stopWords)) txtArr)
  return docTokens

-- | Generate a Map of feature vectors, like (DocTitle -> TermVector)
-- where TermVector represents vector for each document containing bag of words and tf values
--
-- Example:
--
--
generateFeatureVectors :: String -> IO (M.Map DocTitle TermVector)
generateFeatureVectors inputDir = do
  tokenizedDocTermsPair <- tokenize inputDir
  let featureVectorTuples = fmap (\(docTitle,terms) -> (docTitle,mkTermVectorTf terms)) tokenizedDocTermsPair
  let featureVectorsMap = M.fromList (featureVectorTuples)
  return featureVectorsMap

-- | Generate CorpusData containing a map of all the terms in the corpus
-- and their frequencies (how many times each term appear across Documents.
--
-- Example:
--
--
generateCorpusData :: String -> IO CorpusTermsIdf
generateCorpusData inputDir = do
  tokenizedDocTermsPair <- tokenize inputDir
  let featureVectorTuples = fmap (\(docTitle,terms) -> (docTitle,mkTermVectorTf terms)) tokenizedDocTermsPair
  let featureVectorsMap = M.fromList (featureVectorTuples)
  return (mkCorpus featureVectorsMap (length tokenizedDocTermsPair))

-- | Generate TfIdf returned as a Map of (DocTitle -> TermVector) where
-- TermVector contains bagOfWords and each bag of words has tfidf value against the "term"
--
-- Example:
--
--
generateTfIdf :: String -> IO (M.Map DocTitle TermVector)
generateTfIdf inputDir = do
  tokenizedDocTermsPair <- tokenize inputDir
  let featureVectorTuples = fmap (\(docTitle,terms) -> (docTitle,mkTermVectorTf terms)) tokenizedDocTermsPair
  let featureVectorsMap = M.fromList (featureVectorTuples)
  let corpusData = (mkCorpus featureVectorsMap (length tokenizedDocTermsPair))
  return (mkTermVectorTfIdf corpusData featureVectorsMap )



streamingTf :: String -> IO ()
streamingTf input = do
  stopWords <- readStopWordsText ""
  let tfData = TfIdfEnv {stopWords = stopWords}
  let tfidfInitial = TfIdf{ docMap = M.empty, corpusDictionary = M.empty, docCount = 0}
  chainedInputs tfData tfidfInitial input
--  print (debugWord (T.pack "adviser") tfidfInitial)
  return ()

data TfInput = TfInput { tit :: T.Text, txt :: T.Text, env :: TfIdfEnv } deriving Show

mkTfIdfStreaming :: Monad m => Conduit (TfInput) m ByteString
mkTfIdfStreaming = do
  tfi <- await
  case tfi of
    Nothing -> return ()
    Just t -> do case t of
                  TfInput ti tx en          -> yield ( cs (show (ti,doc)) :: ByteString) where
                    terms = tokenizeDoc tx (stopWords en)
                    doc = mkTermVectorTf terms
                  _                         -> return ()
                 mkTfIdfStreaming


chainedInputs :: TfIdfEnv -> TfIdf -> String -> IO ()
chainedInputs tfData tfIdfInit input = runConduitRes
  $ sourceDirectory input
  .| awaitForever (\fp -> prepareInput fp tfData tfIdfInit)
  .| mkTfIdfStreaming
--  .| printC
  .| sinkFile "tmp/op.txt"

prepareInput fp tfData tfidfI = sourceFile fp
  .| decodeUtf8C
  .| mapC ( \x ->  TfInput (T.pack fp) x tfData)

getValue txt (TermVector bow _ _  ) = (txt,M.lookup txt bow)


debugWord txt (TfIdf docMap _ _  ) = M.foldrWithKey (\k (TermVector bow _ _) res -> if (maybePresent (M.lookup txt bow) == 0.0) then res else (M.lookup txt bow) : res ) [] docMap

maybePresent (Just (TfData _ _ v) ) = v
maybePresent Nothing  = 0.0

titleContentsTuple :: (T.Text,String) -> IO (T.Text,T.Text)
titleContentsTuple (t,p) = do
  cont <- TIO.readFile p
  return (t,cont)

loadData path = do
    files <- listDirectory path
    let prefixed = map (\x -> (T.pack x,(path ++ "/") ++ x)) files
    titleDocs <- traverse titleContentsTuple prefixed
    return titleDocs



