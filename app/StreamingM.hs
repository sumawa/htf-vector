--{-# LANGUAGE FlexibleContexts, DeriveGeneric, OverloadedStrings #-}
--module StreamingM where
--
----import TfIdf (TfIdf(..),TfIdfEnv(..), titleDocumentPair, runCorpusDictWithIdf, runComputeTfIdf)
----import Doc (TfData(..),Document(..))
--import Vectorize.TfIdfVector (tokenizeDoc,TfIdf(..), TfIdfEnv(..))
--import qualified Data.Map as M
--import qualified Data.Text as T
--import qualified Data.Text.IO as TIO
--import Data.Char
--import Data.Typeable
--import System.Directory (listDirectory)
----import Data.ByteString.Lazy.Char8 as C -- from utf8-string
--import Data.Word
--
--import qualified Data.List as L
--import Control.Monad.IO.Class
--import Conduit (ConduitT)
--import Conduit
--
--import Control.Monad (forM, forM_, mapM_, mapM)
--import qualified Data.Conduit.List as L
--
--import Data.ByteString    (ByteString)
--import Data.String.Conversions (cs)
--
--import GHC.Generics
--
--main :: IO ()
--main = undefined
--
--streamingTf :: String -> IO ()
--streamingTf input = do
--  stopWords <- readStopWordsText ""
--  let stopWordMap = M.fromList stopWords
--  let tfData = TfIdfEnv {stopWords = stopWordMap}
--  let tfidfInitial = TfIdf{ docMap = M.empty, corpusDictionary = M.empty, docCount = 0}
----  travDirMore1 tfData tfidfInitial input
--  chainedInputs tfData tfidfInitial input
--  print (printWord (T.pack "adviser") tfidfInitial)
--  return ()
--
----data TfInput a = TfInput { tit :: a, txt :: a, env :: TfIdfEnv } deriving Show
----data TfOutput = TfOutput { title :: T.Text, doc :: Document} deriving (Show,Read,Generic)
--data TfInput = TfInput { tit :: T.Text, txt :: T.Text, env :: TfIdfEnv } deriving Show
--
--mkTfIdfStreaming :: Monad m => Conduit (TfInput) m ByteString
--mkTfIdfStreaming = do
--  tfi <- await
--  case tfi of
--    Nothing -> return ()
--    Just t -> do case t of
--                  TfInput ti tx en     -> yield ( cs ( ti <> (T.pack (" ::: \n") <> tx <> (T.pack ("\n ===== \n"))) ) :: ByteString)
--                  _                         -> return ()
--                 mkTfIdfStreaming
--
--
--testListSt = do
--  p <- L.sourceList [ GovOrg 1 "Zas", Individual 2 (Person "Alejandro" "Serrano")] $$ people =$ L.consume
--  print p
--
--chainedInputs :: TfIdfEnv -> TfIdf -> String -> IO ()
--chainedInputs tfData tfIdfInit input = runConduitRes
--  $ sourceDirectory input
--  .| awaitForever (\fp -> prepareInput fp tfData tfIdfInit)
--  .| mkTfIdfStreaming
----  .| printC
--  .| sinkFile "tmp/op.txt"
--
--prepareInput fp tfData tfidfI = sourceFile fp
--  .| decodeUtf8C
--  .| mapC ( \x ->  TfInput (T.pack fp) x tfData)
--
--travDirMore1 :: TfIdfEnv -> TfIdf -> String -> IO ()
--travDirMore1 tfData tfIdfInit input = runConduitRes
----  $ sourceDirectoryDeep True "/Users/sumantawasthi/projs/haskell/htfidf/tfdata/corpora/stopwords/"
----  $ sourceDirectoryDeep True input
--  $ sourceDirectory input
----  .| mapC (\fp -> "--------" ++ (show $ typeOf fp))
----  .| awaitForever (\fp -> handleFile fp)
--  .| awaitForever (\fp -> handleFile fp tfData tfIdfInit)
----  .| mapC (\tfd -> typeOf(tfd))
----  .| mapC (\(txt,doc) -> getValue (T.pack "adviser") doc)
----  .| mapC (\(txt,doc) -> show doc)
--  .| mapC (\(txt,(Document docMap _ _)) -> (M.size docMap))
--  .| printC
--
--handleFile fp tfData tfidfI = sourceFile fp
--  .| decodeUtf8C
--  .| mapC ( \x ->  titleDocumentPair (T.pack fp) x tfData)
----  .| mapC ( \x ->  (fp,(T.splitOn (T.pack "\n") x)))
--
--getValue txt (Document bow _ _  ) = (txt,M.lookup txt bow)
--
--runTfIdfNormal :: String ->  IO ()
--runTfIdfNormal input = do
--  stopWords <- readStopWordsText ""
--  let stopWordMap = M.fromList stopWords
--  let tfData = TfIdfEnv {stopWords = stopWordMap}
--  let tfidfInitial = TfIdf{ docMap = M.empty, corpusDictionary = M.empty, docCount = 0}
----  let testStrArr = testStr4
----  let txtArr = (\(t,s) -> (T.pack t, T.pack s)) <$> testStrArr
----  txtArr <- loadData "/Users/sumantawasthi/data/test/bbc/tech"
--  txtArr <- loadData input
--  print $ typeOf txtArr
--  let txtDocList = (fmap (\(k,t) -> titleDocumentPair k t tfData) txtArr)
--  let txtDocs = M.fromList (txtDocList)
----  print txtDocList
--  let tfidfComputed = TfIdf{ docMap = txtDocs, corpusDictionary = M.empty, docCount = 0}
--  print txtDocList
----  print (printWord (T.pack "adviser") tfidfComputed)
----  print (printWord (T.pack "Kyrgyz") tfidfComputed)
--
--postProc tfidfL =   do
--  print "IN POST"
--  let tfidfComputed = TfIdf{ docMap = M.fromList [tfidfL], corpusDictionary = M.empty, docCount = 0}
--  print (printWord (T.pack "Kyrgyz") tfidfComputed)
--
--
--data Client i = GovOrg  { clientId :: i, clientName :: String }
--              | Company { clientId :: i, clientName :: String
--                         , person :: Person, duty :: String }
--              | Individual { clientId :: i, person :: Person }
--              deriving Show
--
--data Person = Person { firstName :: String, lastName  :: String }
--              deriving (Show, Read, Generic)
--
--
--people :: Monad m => Conduit (Client i) m Person
--people = do client <- await
--            case client of
--              Nothing -> return ()
--              Just c -> do case c of
--                             Company { person = p }    -> yield p
--                             Individual { person = p } -> yield p
--                             _                         -> return ()
--                           people
--
---- EXXXXXX
--
--runTfIdfStreaming :: IO ()
--runTfIdfStreaming =  do
--  stopWords <- readStopWordsText ""
--  let stopWordMap = M.fromList stopWords
--  let tfData = TfIdfEnv {stopWords = stopWordMap}
--  processTfIdfStreaming tfData
--
--processTfIdfStreaming :: TfIdfEnv ->  IO ()
--processTfIdfStreaming env =
--  runConduitRes $ sourceDirectoryDeep True "/Users/sumantawasthi/data/test/bbc/tech"
--  .| awaitForever sourceFile
--  .| decodeUtf8C .| peekForeverE (do
--    liftIO $ print "NEW FILE??"
----    c <- lineC
--    len <- lineC lengthCE
----    let txtArr = ["T",line]
--    liftIO $ print len)
--
--
--travDirMore :: IO ()
--travDirMore = runConduitRes
--     $ sourceDirectoryDeep True "/Users/sumantawasthi/projs/haskell/htfidf/tfdata/corpora/stopwords/"
--     .| mapC (\fp ->  fp )
----    .| filterC (\fp -> takeExtension fp == ".hs")
--    .| awaitForever (\x ->  sourceFile x)
--    .| decodeUtf8C
--    .| mapC (T.map toUpper)
--    .| encodeUtf8C
--    .| stdoutC
--
--loadDataStreaming path = runConduitRes
--  $ sourceDirectoryDeep True "/Users/sumantawasthi/projs/haskell/htfidf/tfdata/corpora/stopwords/"
--  .| awaitForever sourceFile
--  .| mapC (\x ->  x)
--  .| printC
--
--
--runTfIdfS :: IO ()
--runTfIdfS = do
--  stopWords <- readStopWordsText ""
--  let stopWordMap = M.fromList stopWords
--  let tfData = TfIdfEnv {stopWords = stopWordMap}
----  let testStrArr = testStr4
----  let txtArr = (\(t,s) -> (T.pack t, T.pack s)) <$> testStrArr
--  txtArr <- loadData "/Users/sumantawasthi/data/test/bbc/tech"
--  print $ typeOf txtArr
--  let txtDocList = (fmap (\(k,t) -> titleDocumentPair k t tfData) txtArr)
--  let txtDocs = M.fromList (txtDocList)
----  print txtDocList
--  let tfidf = TfIdf{ docMap = txtDocs, corpusDictionary = M.empty, docCount = length txtArr}
--  let tfidfWithIdf = runCorpusDictWithIdf tfidf
----  print (corpusDictionary tfidfWithIdf)
--  let tfidfComputed = runComputeTfIdf tfidfWithIdf
----  print (corpusDictionary tfidfComputed)
--  print ("-------")
--  print ("-------")
--  print (M.take 1 (docMap tfidfComputed))
----  print (M.size (docMap tfidfComputed))
--  print ("******")
--  print ("******")
--  print ("******")
--  print (printWord (T.pack "adviser") tfidfComputed)
----  print (printWord (T.pack "spread") tfidfComputed)
----  print (M.lookup (T.pack "juice") (docMap tfidfComputed))
--  return ()
--
--printWord txt (TfIdf docMap _ _  ) = M.foldrWithKey (\k (Document bow _ _) res -> if (maybePresent (M.lookup txt bow) == 0.0) then res else (M.lookup txt bow) : res ) [] docMap
--
--maybePresent (Just (TfData _ _ v) ) = v
--maybePresent Nothing  = 0.0
--
--titleContentsTuple :: (T.Text,String) -> IO (T.Text,T.Text)
--titleContentsTuple (t,p) = do
--  cont <- TIO.readFile p
--  return (t,cont)
--
----loadData :: FilePath -> [FilePath] -> IO [(String, [(String, Int)])]
----loadData :: FilePath -> [FilePath] -> IO [String]
--loadData path = do
--    files <- listDirectory path
----    print files
--    let prefixed = map (\x -> (T.pack x,(path ++ "/") ++ x)) files
----    print prefixed
--    titleDocs <- traverse titleContentsTuple prefixed
----    print (typeOf titleDocs)
--    return titleDocs
--
--readStopWordsText stopFile =  do
--  ls <- if (all isSpace stopFile) then
--          fmap T.lines (TIO.readFile "./tfdata/corpora/stopwords/english")
--        else fmap T.lines (TIO.readFile stopFile)
--  return (fmap (\x -> (x,x)) ls)
--
--testStr1 = [("T1","the man went out for a walk"),("T2","the children sat around the fire")]
--testStr2 = [("T1","It is going to rain today."),("T2","Today I am not going outside."),("T3","I am going to watch the season premiere.")]
--testStr3 = [("T1","The car is driven on the road"),("T2","The truck is driven on the highway.")]
--testStr4 = [("T1","one flesh one bone one true religion"),("T2","all flesh is grass"),("T3","one is all all is one")]
--
----runTfIdfStreaming :: IO ()
----runTfIdfStreaming =  do
----  stopWords <- readStopWordsText ""
----  let stopWordMap = M.fromList stopWords
----  let tfData = TfIdfEnv {stopWords = stopWordMap}
----  runConduitRes $ sourceFile "/Users/sumantawasthi/data/test/bbc/tech" .| decodeUtf8C .| peekForeverE (do
----    len <- lineC lengthCE
------    let txtArr = ["T",line]
----    liftIO $ print len)
--
