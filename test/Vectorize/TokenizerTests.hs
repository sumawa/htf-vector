{-# LANGUAGE OverloadedStrings #-}
module Vectorize.TokenizerTests where

import Prelude hiding (negate, sum)
import Test.QuickCheck ( Property, (==>) )
import Test.QuickCheck.Property ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import qualified Data.Text as T

import Vectorize.Tokenizer
import TestData.TokenizerData

import DataTypes.TfIdfTypes 

import UnitAssertionsHelper

tests :: TestTree
tests = testGroup "TOKENIZER TESTS"
        [
           testGroup "tokenizer data" $ map (assert2ParamGeneric tokenizeDoc)
            [ ("checking if test present", testTxt1, stopSet, expectedTok1)
--            , ("", "a",    mkTermVectorTf ["a", "test"], (TfData 1 1 0.0))
--            , ("", "a",    mkTermVectorTf ["test"], (TfData 1 1 0.0))
            ]
        ]

