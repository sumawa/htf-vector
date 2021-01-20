-- | Utility module for assertions based on function with number of params and result types
module UnitAssertionsHelper where

import Control.Monad (unless)
import Test.HUnit      ( (@=?), Assertion, assertFailure )
import Test.Tasty.HUnit (testCase)
import Test.Tasty (TestTree)

assert2ParamDouble :: (Show a, Show b) => (a -> b -> Double) -> (String, a, b, Double) -> TestTree
assert2ParamDouble fn (descr, in1, in2, expectedOutCome) =
    testCase (descr++" [input: "++show in1++"," ++show in2++"]") assert
        where assert = assertApproxEquals "" 0.001 expectedOutCome $ fn in1 in2

assert3ParamGeneric :: (Show a, Show b, Show c, Show d, Eq d)
         => (a -> b -> c -> d)
         -> (String, a, b, c, d)
         -> TestTree
assert3ParamGeneric fn (descr, in1, in2, in3, expectedOutCome) =
    testCase (descr++" [input: "++show in1++"," ++show in2++"," ++show in3++"]") assert
        where assert = expectedOutCome @=? fn in1 in2 in3

assert3ParamDouble :: (Show a, Show b, Show c)
         => (a -> b -> c -> Double)
         -> (String, a, b, c, Double)
         -> TestTree
assert3ParamDouble fn (descr, in1, in2, in3, expectedOutCome) =
    testCase (descr++" [input: "++show in1++"," ++show in2++"," ++show in3++"]") assert
        where assert = assertApproxEquals "" 0.001 expectedOutCome $ fn in1 in2 in3

assert2ParamGeneric :: (Show a, Show b, Show c, Eq c) => (a -> b -> c) -> (String, a, b, c) -> TestTree
assert2ParamGeneric fn (descr, in1, in2, expectedOutCome) =
    testCase (descr++" [input: "++show in1++"," ++show in2++"]") assert
        where assert = expectedOutCome @=? fn in1 in2

assert1ParamGeneric :: (Show a, Show b, Eq b) => (a -> b) -> (String, a, b) -> TestTree
assert1ParamGeneric fn (descr, input, expectedOutCome) =
    testCase (descr++" [input: "++show input++"]") assert
        where assert = expectedOutCome @=? fn input

assert1ParamDouble :: Show a => (a -> Double) -> (String, a, Double) -> TestTree
assert1ParamDouble fn (descr, input, expectedOutCome) =
    testCase (descr++" [input: "++show input++"]") assert
        where assert = assertApproxEquals "" 0.001 expectedOutCome $ fn input

assertApproxEquals :: String  -- ^ The message prefix
                  -> Double  -- ^ The maximum difference between expected and actual
                  -> Double  -- ^ The expected value
                  -> Double  -- ^ The actual value
                  -> Assertion
assertApproxEquals preface delta expected actual =
  unless (abs (expected - actual) < delta) (assertFailure msg)
 where msg = (if null preface then "" else preface ++ "\n") ++
             "expected: " ++ show expected ++ "\n but got: " ++ show actual
