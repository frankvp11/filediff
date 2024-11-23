{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

-- imports

import Test.Tasty
import Test.Tasty.HUnit

import Data.Monoid
import Control.Monad
import Control.Monad.Trans.Either
import Control.Monad.IO.Class (liftIO)

-- qualified imports

import qualified Data.Text as T

import qualified System.IO as IO
import qualified System.Directory as D

-- imported functions

import Data.List ((\\))

import System.Exit (exitSuccess)

import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Time.Calendar (toGregorian)

import Control.Monad ((>>=), return, when)
import Control.Applicative ((<$>), (<*>))
import Control.Monad.IO.Class (liftIO)

import Data.Either.Combinators (isLeft, fromLeft)

import qualified Filediff as F
import qualified Filediff.Types as F



testListDiffEdgeCase1 :: Assertion
testListDiffEdgeCase1 = do
    return $ F.diffLists "" "wabxyze"
    True @?= True -- no exception: considered success for this test

testListDiffEdgeCase2 :: Assertion
testListDiffEdgeCase2 = do
    return $ F.diffLists "wabxyze" ""
    True @?= True -- no exception: considered success for this test

testListDiffEdgeCase3 :: Assertion
testListDiffEdgeCase3 = do
    return $ F.diffLists "" ""
    True @?= True -- no exception: considered success for this test


testListDiffComposition :: Assertion
testListDiffComposition = do
    let a = "abcdefg"
    let b = "wabxyze"
    let c = "#x##ye"

    let ab = F.diffLists a b
    let bc = F.diffLists b c
    let ac = F.diffLists a c

    ab `mappend` bc @?= ac

testListDiffCompositionEdgeCase1 :: Assertion
testListDiffCompositionEdgeCase1 = do
    let a = ""
    let b = "bbb"
    let c = ""

    let ab = F.diffLists a b
    let bc = F.diffLists b c
    let ac = F.diffLists a c

    ab `mappend` bc @?= ac

testListDiffCompositionEdgeCase2 :: Assertion
testListDiffCompositionEdgeCase2 = do
    let ab = F.ListDiff {F.dels = [], F.adds = [(0,"a")]}
    let bc = F.ListDiff {F.dels = [], F.adds = [(1,"b")]}

    let ac = F.ListDiff {F.dels = [], F.adds = [(0,"a"),(1,"b")]}

    ab `mappend` bc @?= ac



testListApplyEdgeCase1 :: Assertion
testListApplyEdgeCase1 = do
    let base = ""
    let comp = "abcde"

    let listdiff = F.diffLists base comp
    let applied = F.applyListDiff listdiff base
    applied @?= Right comp


testListDiffApplyFailureDeletionCase :: Assertion
testListDiffApplyFailureDeletionCase = do
    let base = "abcdefg"
    let faultyBase = "ab*defg"
    let comp = "wabxyze"

    let listDiff = F.diffLists base comp
    let eitherResult = F.applyListDiff listDiff faultyBase
    eitherResult @?= Left "Fatal: couldn't apply list diff (application requires removing an element where the diff calls for a different element residing at that index)."

testListDiffApplyFailureAdditionCase :: Assertion
testListDiffApplyFailureAdditionCase = do
    let base = "abcdefg"
    let faultyBase = "abcde"
    let comp = "wabxyzefgq"

    let listDiff = F.diffLists base comp
    let eitherResult = F.applyListDiff listDiff faultyBase
    eitherResult @?= Left "Fatal: couldn't apply list diff (application requires inserting at an index larger than the length of the list to which to apply the diff)."
    return ()


testSameListConcatenated :: Assertion
testSameListConcatenated = do
    let diff = F.diffLists "abc" "abcabc"

    diff @?= F.ListDiff {F.dels = [], F.adds = [(3,'a'),(4,'b'),(5,'c')]}

testSameListConcatenatedWithIntermediate :: Assertion
testSameListConcatenatedWithIntermediate = do
    let diff = F.diffLists "abc" "abc*abc"

    diff @?= F.ListDiff {F.dels = [], F.adds = [(3, '*'), (4,'a'),(5,'b'),(6,'c')]}

customCase = do 
    let base = "abcdef"
    let comp = "wabxyze"
    let diff = F.diffLists base comp
    let applied = F.applyListDiff diff base

    -- print the diffs
    print diff
    print applied

    -- check if the applied diff is equal to the comp




tests :: TestTree
tests = testGroup "unit tests"
    [ -- diffing
    testCase
        "Testing list diffing (edge case 1)"
        (testListDiffEdgeCase1)
    , testCase
        "Testing list diffing (edge case 2)"
        (testListDiffEdgeCase2)
    , testCase
        "Testing list diffing (edge case 3)"
        (testListDiffEdgeCase3)
    , testCase
        "Testing sequence patching (edge case 1)"
        (testListApplyEdgeCase1)

    --     composition
    , testCase
        "Testing list diffing composition"
        (testListDiffComposition)
    , testCase
        "Testing list diffing composition (edge case 1)"
        (testListDiffCompositionEdgeCase1)
    , testCase
        "Testing list diffing composition (edge case 2)"
        (testListDiffCompositionEdgeCase2)
    , testCase
        "Testing same list concatenated with itself (case 1)"
        testSameListConcatenated
    , testCase
        "Testing same list concatenated with itself (case 2)"
        testSameListConcatenatedWithIntermediate

    , testCase
        "Custom case"
        customCase
        

    ]

    -- run the custom case
main :: IO ()
main = defaultMain tests
