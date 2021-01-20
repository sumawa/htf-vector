{-# LANGUAGE OverloadedStrings #-}
module Vectorize.VectorAPITests where

import Prelude hiding (negate, sum)
import Test.QuickCheck ( Property, (==>) )
import Test.QuickCheck.Property ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import qualified Data.Text as T
import qualified Vectorize.TfIdfVector as TV

import Vectorize.TfIdfVector
import DataTypes.TfIdfTypes 

import UnitAssertionsHelper

tests :: TestTree
tests = testGroup "Tf Idf TfIdf"
        [ -- testGroup "Dot Products" $ map (genTestF2 dotProd)
        --   [ ("3-4-5", [1,3,-5], [4, -2, -1], 3)
        --   , ("identical", [1], [1], 1.0)
        --   , ("orthogonal", [1,0], [0,1], 0)
        --   ]
        -- , testGroup "cosines" $ map (genTestF2 cosVec)
        --   -- ("identical", [0], [0], NaN) -- can't determine the angle.
        --   [ ("identical", [1], [1], 1.0)
        --   , ("identical", [1,2], [1,2], 1.0)
        --   , ("orthogonal", [1,0], [0,1], 0)
        --   ]
        -- , testGroup "Magnitude tests" $ map (genTest magnitude)
        --   [ ("3-4-5", [3,4], 5)
        --   , ("empty", [], 0)
        --   , ("single", [1], 1)
        --   ]
         testGroup "tf tests" $ map (assert2ParamGeneric getTfFromTermVector)
          [ ("checking if test present", "test", mkTermVectorTf ["test"], (TfData 1 1 0.0))
          , ("", "a",    mkTermVectorTf ["a", "test"], (TfData 1 1 0.0))
          , ("", "a",    mkTermVectorTf ["test"], (TfData 1 1 0.0))
          ]
        
--         , testGroup "tf tests" $ map (genTest2 tf)
--          [ ("", "test", mkDocument ["test"], 1)
--          , ("", "a",    mkDocument ["a", "test"], 1)
--          , ("", "a",    mkDocument ["test"], 0)
--          ]
--        , testGroup "idf tests" $ map (genTestF2 idf)
--          [ ("", "test", mkCorpus [["test"]], log(2/3))
--          , ("", "a", mkCorpus [["test"]], log(1))
--          , ("", "a", mkCorpus [["a", "test"],["test"]], log(2/2))
--          ]
--        , testGroup "tf_idf tests" $ map (genTestF3 tf_idf)
--          [ ("", "test", mkDocument ["test"], mkCorpus [["test"]], log(2/3))
--          , ("", "a",    mkDocument ["a", "test"], mkCorpus [["some"], ["test"]], 0.40546)
--          , ("", "foo",  mkDocument ["foo"], mkCorpus [["test"]], 0)
--          , ("", "foo",  mkDocument ["foo"], mkCorpus [["foo"],["test"]], 0)
--          , ("", "bar",  mkDocument ["foo"], mkCorpus [["test"]], 0)
--          , ("", "bar",  mkDocument ["foo"], mkCorpus [["foo"],["test"]], 0)
--          ]
--        , testGroup "Similarity tests, trivial corpus" $
--            map (genTest2 $ sim $ mkCorpus [["test"]])
--                    [ ("same doc", "test", "test", 1)
--                      -- This next test is invalid becausse the
--                      -- initial smoothing causes funny results (this
--                      -- should not be 1.0, and it /is not/ when the
--                      -- corpus is bigger.)
--                    , ("one off", "a test", "the test", 1.0)
--                    , ("No match", "foo", "bar", 0.0)
--                    ]
--        , testGroup "Similarity tests, minor corpus" $
--            map (genTestF2 $ sim $ mkCorpus [ ["a", "sample"]
--                                            , ["the", "test"]
--                                            , ["big", "example"]
--                                            , ["more", "terms"]])
--                    [ ("same doc", "test", "test", 1)
--                    , ("one off", "a test", "the test", 0.5)
--                    , ("No match", "foo", "bar", 0.0)
--                    ]
--        , testProperty "idf /= NaN" prop_idfIsANum
--        , testProperty "tf_idf /= NaN" prop_tf_idfIsANum
--        , testProperty "similarity /= NaN" prop_similarity_isANum
--        , testProperty "v + 0 = v" prop_addVectorZero
--        , testProperty "v - v = 0" prop_negateVector
        ]

--prop_addVectorZero :: TermVector -> Bool
--prop_addVectorZero v = addVectors v zeroVector == v &&
--                       addVectors zeroVector v == v

--prop_negateVector :: TermVector -> Bool
--prop_negateVector v =
--    let theSum = addVectors v (negate v)
--    in and [ TV.lookup k theSum == 0
--           | k <- TV.keys v
--           ]

--prop_sum :: [TermVector] -> Bool
--prop_sum vs =
--    foldr addVectors zeroVector vs == sum vs

--prop_idfIsANum :: String -> [[String]] -> Bool
--prop_idfIsANum term docs = not (isNaN (idf termTxt $ mkCorpus docsTxt))
--  where
--    termTxt = T.pack term
--    docsTxt = map (map T.pack) docs
--
--prop_tf_idfIsANum :: String -> [String] -> [[String]] -> Bool
--prop_tf_idfIsANum term doc docs = not $ isNaN $ tf_idf termTxt (mkDocument docTxt) $ mkCorpus docsTxt
--  where
--    termTxt = T.pack term
--    docTxt = map T.pack doc
--    docsTxt = map (map T.pack) docs
--
--prop_similarity_isANum :: [[String]] -> [String] -> [String] -> Property
--prop_similarity_isANum strCorp d1 d2 = strCorp /= [] &&
--                                       (concat d1 /= []) &&
--                                       (concat d2 /= [])==> let
--  corpus = mkCorpus $ map (map T.pack) strCorp
--  doc1 = map T.pack d1
--  doc2 = map T.pack d2
--  in not $ isNaN $ similarity corpus doc1 doc2