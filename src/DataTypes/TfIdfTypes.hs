{-# LANGUAGE FlexibleContexts, DeriveGeneric, OverloadedStrings #-}

module DataTypes.TfIdfTypes (
  TfData(..)
  , Term
  , DocTitle
  , IdfData(..)
  , TermVector(..)
  , CorpusData(..)
  , TfIdfEnv(..)
) where
-- Using Generics For Future use of lenses
import GHC.Generics
import qualified Data.Text as T
import qualified Data.Set as S
import qualified Data.Map as M

-- synonym for text will be used everywhere, a basic unit
type Term = T.Text
-- synonym for the title of given document containing Terms(s)
type DocTitle = T.Text
-- | TfData : Term frequency data which has 3 components can be used to
-- prepare a detailed data vector for each document

-- | Note: It refers closely to to SOLR's  "Term vector"
-- which is one of the key concept in document search for SOLR using query tokens.
data TfData = TfData {
  tfCount :: Double           -- ^ The count of terms within the document
  , tf :: Double              -- ^ term frequency computed using various formulas
  , tfidf :: Double           -- ^ The final "TfIdf" value of the term within the document "within" the "corpus"
  } deriving (Show,Eq,Read,Generic)

instance Semigroup (TfData) where
  (TfData d11 d12 d13) <> (TfData d21 d22 d23) = TfData (d11+d21) (d12+d22) (d13+d23)

-- | IdfData : The data representing the importance of the term in entire corpus

-- | The inverse document frequency is a measure of how much information the word provides,
-- i.e., if it's common or rare across all documents or popularity of the term within corpus (wikipedia definition)
-- It is used in Corpus data to represent "Bag of Words" using a "Map" for each term
data IdfData = IdfData {
  dfCount :: Double           -- ^ The count of terms within the "corpus" across all documents
  , idf :: Double             -- ^ It is the logarithmically scaled inverse fraction of the documents that contain the word (from wikipedia)
  } deriving (Read,Show,Eq,Generic)

-- | TermVector : Represent each "document" or "sentence" or "collection of words" which is being vectorized
-- and typically used in some kind of mathematical evaluation like evaluating similiarities between text documents.

-- | For example comparing two documents and finding out
-- how "similar" they are using a mathematical model using vectors.
-- Each document will have a bag of words "token" or "term" and its TfData (mentioned above)
data TermVector = TermVector{
    bagOfWords :: M.Map Term TfData -- ^ map of "tokens" and their TfData (count, tf value, and final tfidf value)
    , docWordCount :: Int           -- ^ The count of terms within this document
    , vectorLength :: Double        -- ^ will be used in computing cosine similarity in future.
} deriving (Read, Eq, Generic)

instance Show TermVector where
  show (TermVector bow docWc vl) = show $ T.intercalate (T.pack " --  ") txts where
    txts = M.foldrWithKey (\k v res -> (k <> strToTxt "," <> strToTxt (show (tf v)) <> strToTxt "," <>  T.pack (show (tfidf v))  ) : res  ) [] bow

strToTxt str = T.pack str

-- | Corpus : Represent "corpus" of "documents" contains a dictionary of terms with their IdfData values
-- 
-- IdfData is a container having "dfCount" which is the count of documents within corpus
-- in which the term exists and a computed "idf" value which is inverse document frequency computed
-- after the CorpusData is built completely
data CorpusData = CorpusData {
  dictFreq :: M.Map Term IdfData  -- ^ map of "terms" and their Idfata (count of documents in which , tf value, and final tfidf value)
  , docCount :: Int
} deriving (Show)

newtype TfIdfEnv = TfIdfEnv {
  stopWords :: S.Set Term
} deriving (Show)


