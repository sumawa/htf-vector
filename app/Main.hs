{-# LANGUAGE FlexibleContexts, DeriveGeneric, OverloadedStrings #-}
module VectorMain where

import Vectorize.TfIdfVector (TfIdf(..), mkTermVectorTf, mkCorpusIdf, mkTermVectorTfIdf)
import Vectorize.Tokenizer (tokenizeDoc,readStopWordsText)
import DataTypes.TfIdfTypes (Document(..),TfData(..), Term,TfIdfEnv(..))
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

main :: IO ()
main = undefined

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

getValue txt (Document bow _ _  ) = (txt,M.lookup txt bow)

runTfIdfNormal :: String ->  IO ()
runTfIdfNormal input = do
  stopWords <- readStopWordsText ""
--  let tfData = TfIdfEnv {stopWords = stopWords}
  let tfidfInitial = TfIdf{ docMap = M.empty, corpusDictionary = M.empty, docCount = 0}
--  let testStrArr = testStr4
--  let txtArr = (\(t,s) -> (T.pack t, T.pack s)) <$> testStrArr
--  txtArr <- loadData "/Users/sumantawasthi/data/test/bbc/tech"
  txtArr <- loadData input
--  print $ typeOf txtArr
  let txtDocList = (fmap (\(k,t) -> (k,mkTermVectorTf (tokenizeDoc t stopWords) )) txtArr)
--  TIO.writeFile "tmp/normal.txt" (T.pack (show txtDocList))
  let txtDocs = M.fromList (txtDocList)
--  print txtDocs
  let tfidf = TfIdf{ docMap = txtDocs, corpusDictionary = M.empty, docCount = 0}
  let tfidfWithIdf = mkCorpusIdf tfidf
  let tfidfComputed = mkTermVectorTfIdf tfidfWithIdf
--  print (corpusDictionary tfidfComputed)
  TIO.writeFile "tmp/normal.txt" (T.pack $ show (docMap tfidfComputed))
  return ()

debugWord txt (TfIdf docMap _ _  ) = M.foldrWithKey (\k (Document bow _ _) res -> if (maybePresent (M.lookup txt bow) == 0.0) then res else (M.lookup txt bow) : res ) [] docMap

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

testStr1 = [("T1","the man went out for a walk"),("T2","the children sat around the fire")]
testStr2 = [("T1","It is going to rain today."),("T2","Today I am not going outside."),("T3","I am going to watch the season premiere.")]
testStr3 = [("T1","The car is driven on the road"),("T2","The truck is driven on the highway.")]
testStr4 = [("T1","one flesh one bone one true religion"),("T2","all flesh is grass"),("T3","one is all all is one")]


