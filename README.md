# htfidf

Htfidf is a library for generating term vectors (TfIdf)
https://en.wikipedia.org/wiki/Tf%E2%80%93idf

[comment]: <> Docs here: <link href="https://htmlpreview.github.io/?https://github.com/sumawa/htf-vector/blob/master/docindex.htm target="_blank">

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

```haskell
#!/usr/bin/env stack
-- stack script --resolver lts-16.28
import Vectorize.Tokenizer (tokenizeDoc)

main = do
    -- Tokenize based on either externall provide stop words or "" for default
    let testStrArr = [("T1","one flesh one bone one true religion"),("T2","all flesh is grass"),("T3","one is all all is one")]
    let txtDocList = (fmap (\(k,t) -> (k,mkTermVectorTf (tokenizeDoc t tfData) )) txtArr)
    let tfidfInitial = TfIdf{ docMap = M.empty, corpusDictionary = M.empty, docCount = 0}
    -- Tokenize sentences
    -- Pending
    
    return ()
```

## Libraries

Description Pending

## Streaming version

How to use streaming version. Pending

``` haskell
#!/usr/bin/env stack
-- stack script --resolver lts-16.28
{-# LANGUAGE ExtendedDefaultRules #-}
import Vectorize.Tokenizer (tokenizeDoc)

main :: IO ()
main = do
    putStrLn "Streaming version:"
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

