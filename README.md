# htf-vector

Htf-Vector is a library for vectorizing sentences. Such vectors are useful in 
various use cases. 

Some of the reasons you'd like to use htf-vector are:

* vectorize blah blah
* vectorize blah blah
* Streaming version

Check out documentation
[Documentation](https://en.wikipedia.org/wiki/Tf%E2%80%93idf).

__NOTE__ This is a WIP project

## Table of Contents ##
1. [Synopsis](#synopsis)
2. [Libraries](#libraries)
3. [Streaming version](#streaming-version)

## Synopsis

Basic examples of tf-vector usage, much more to follow!

```haskell
#!/usr/bin/env stack
-- stack script --resolver lts-16.28
import Vectorize.Tokenizer (tokenizeDoc)

main = do
    -- Pure operations: summing numbers.
    let testStrArr = [("T1","one flesh one bone one true religion"),("T2","all flesh is grass"),("T3","one is all all is one")]
    let txtDocList = (fmap (\(k,t) -> (k,mkTermVectorTf (tokenizeDoc t tfData) )) txtArr)
    let tfidfInitial = TfIdf{ docMap = M.empty, corpusDictionary = M.empty, docCount = 0}
    -- Tokenize sentences
    let txtDocList = (fmap (\(k,t) -> (k,mkTermVectorTf (tokenizeDoc t tfData) )) txtArr)
    
    return ()
```

## Libraries

Blah blah 

## Streaming version

How to use streaming version

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

Blah blah

* blah blah
* blah blah
