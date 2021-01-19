# htfidf

Htfidf is a library for generating term vectors (TfIdf)
https://en.wikipedia.org/wiki/Tf%E2%80%93idf

Term frequency-inverse document frequency (TF-IDF) term vectors are often used to represent text documents.

Term vectors can be used in 

* performing text mining and machine learning operations. 
* document searching
* recommendations
* various text analysis.

Wiki
[Documentation](https://en.wikipedia.org/wiki/Tf%E2%80%93idf).

__NOTE__ This project is WIP

## Table of Contents ##
1. [Synopsis](#synopsis)
2. [Libraries](#libraries)
3. [Streaming version](#streaming-version)

## Synopsis

Basic examples of htfidf usage, much more to follow!

One can use the steps to experiment with individual steps towards vectorization (tokenize, tf computation, corpus etc.)

Or one can use directly available apis from ```Vectorize.VectorAPI``` packages.

```haskell
#!/usr/bin/env stack
-- stack script --resolver lts-16.28
import Vectorize.Tokenizer (tokenizeDoc)
import Vectorize.VectorAPI (tokenize,

import qualified Data.Map as M
import DataTypes.TfIdfTypes (DocTitle,TermVector(..)) 
import Vectorize.VectorAPI (tokenize)
import Vectorize.TfIdfVector (mkTermVectorTf, mkCorpus, mkTermVectorTfIdf)

exampleTfIdf :: String -> IO (M.Map DocTitle TermVector)
exampleTfIdf inputDir = do
  -- tokenize and generate pairs of (document title, [terms]) 
  tokenizedDocTermsPair <- tokenize inputDir
  
  -- generate feature vectors of document title -> TermVector 
  let featureVectorTuples = fmap (\(docTitle,terms) -> (docTitle,mkTermVectorTf terms)) tokenizedDocTermsPair
  let featureVectorsMap = M.fromList (featureVectorTuples)
  
  -- generate corpus data for all the documents
  let corpusData = (mkCorpus featureVectorsMap (length tokenizedDocTermsPair))
  
  -- generate tfidf values map of "document title" -> TermVector
  return (mkTermVectorTfIdf corpusData featureVectorsMap )
  
```

## Libraries

Generated Hadoock documentation, figuring out how to present the link. 

## Streaming version

How to use streaming version. Pending

``` haskell
#!/usr/bin/env stack
-- stack script --resolver lts-16.28
{-# LANGUAGE ExtendedDefaultRules #-}
import Vectorize.Tokenizer (tokenizeDoc)

main :: IO ()
main = do
    putStrLn "Streaming version is Work in pogress:"
    chainedInputs tfData tfidfInitial input
    return ()
    
chainedInputs :: TfIdfEnv -> TfIdf -> String -> IO ()
chainedInputs tfData tfIdfInit input = runConduitRes
  $ sourceDirectory input
  .| awaitForever (\fp -> prepareInput fp tfData tfIdfInit)
  .| mkTfIdfStreaming
--  .| printC
  .| sinkFile "tmp/op.txt"
    
```

