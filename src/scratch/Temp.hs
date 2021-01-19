module Temp where

import qualified Data.Map as M
import DataTypes.TfIdfTypes (DocTitle,TermVector(..)) 
import Vectorize.VectorAPI (tokenize)
import Vectorize.TfIdfVector (mkTermVectorTf, mkCorpus, mkTermVectorTfIdf)

generateTfIdf :: String -> IO (M.Map DocTitle TermVector)
generateTfIdf inputDir = do
  tokenizedDocTermsPair <- tokenize inputDir
  let featureVectorTuples = fmap (\(docTitle,terms) -> (docTitle,mkTermVectorTf terms)) tokenizedDocTermsPair
  let featureVectorsMap = M.fromList (featureVectorTuples)
  let corpusData = (mkCorpus featureVectorsMap (length tokenizedDocTermsPair))
  return (mkTermVectorTfIdf corpusData featureVectorsMap )
