{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

-- | The module exposing the functionality of this package
module Filediff
( -- * Lists
  diffLists
, applyListDiff

) where

import Debug.Trace
import qualified Data.HashMap as HMap

import Control.Concurrent (forkIO)
import Control.Concurrent.Thread as Thread (Result(..), result)
import Control.Concurrent.Thread.Group as ThreadGroup (new, forkIO, wait)

import qualified System.IO as IO
import qualified System.Directory as D

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import qualified Data.MemoCombinators as Memo

import Data.Either.Combinators

-- Function imports

import Data.Maybe (isJust, fromJust, catMaybes)
import Data.List ((\\), intersect, sort)
import Data.Monoid
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Either
import Control.Monad.IO.Class (liftIO)

-- Filediff imports

import Filediff.Types


-- | Computes the minimal number of additions and deletions needed to
--   transform the first parameter into the second.
--
--       > λ diffLists "abcdefg" "wabxyze"
--       > ListDiff {dels = [2,3,5,6], adds = [(0,'w'),(3,'x'),(4,'y'),(5,'z')]}
diffLists :: forall a. (Eq a) => [a] -> [a] -> ListDiff a
diffLists a b = ListDiff
    (nonSubsequenceIndices common a)
    (getProgressiveIndicesToAdd common b)
    where
        common :: [a]
        common = longestCommonSubsequenceWrapper a b

        -- | Generates the list of additions as [(Int, a)] where Int is the index to add.
        getProgressiveIndicesToAdd :: [a] -> [a] -> [(Int, a)]
        getProgressiveIndicesToAdd sub super =
            map (\i -> (i, super !! i)) $ nonSubsequenceIndices sub super


-- | Applies a list diff. For example,
--
--       > ListDiff {dels = [2,3,5,6], adds = [(0,'w'),(3,'x'),(4,'y'),(5,'z')]}
--       > λ applyListDiff it "abcdefg"
--       > Right "wabxyze"
--
-- Returns a fail state if the diff cannot be applied. This can happen
-- for two reasons: first, the diff calls for a deletion at an index
-- but the index is out of bounds. Second, it can happen if the diff calls
-- for an element to be added at an index too large for the given input.
-- Here are respective examples of inputs that would trigger this case:
--
--     > let base = "abcdefg"
--     > let faultyBase = "abdefg"
--     > let comp = "wabxyze"
--     > let listDiff = F.diffLists base comp
--     > F.applyListDiff listDiff faultyBase -- fails
--
-- and
--
--     > let base = "abcdefg"
--     > let faultyBase = "abcde"
--     > let comp = "wabxyzefgq"
--     > let listDiff = F.diffLists base comp
--     > F.applyListDiff listDiff faultyBase -- fails
applyListDiff :: forall a. (Eq a) => ListDiff a -> [a] -> Either Error [a]
applyListDiff (ListDiff dels adds) list =
    removeAtIndices dels list >>= insertAtProgressiveIndices adds


-- | Inserts elements at the specified progressive indices.
--   Example:
--
--       > insertAtProgressiveIndices [(1,'a'),(3,'b')] "def"
--       > Right "daebf"
insertAtProgressiveIndices :: [(Int, a)] -> [a] -> Either Error [a]
insertAtProgressiveIndices = insertAtProgressiveIndices' 0

insertAtProgressiveIndices' :: Int -> [(Int, a)] -> [a] -> Either Error [a]
insertAtProgressiveIndices' _ [] dest = Right dest
insertAtProgressiveIndices' curr src@((i,s):src') dest =
    if i == curr
        then (:) s <$> insertAtProgressiveIndices' (succ curr) src' dest
        else case dest of
            (d:dest') -> (:) d <$> insertAtProgressiveIndices' (succ curr) src dest'
            [] -> Left "Fatal: couldn't apply list diff (application requires inserting at an index larger than the length of the list to which to apply the diff)."


-- | Best explained by example:
--
--       > subsequenceIndices "abe" "abcdefg"
--       > [0,1,4]
-- subsequenceIndices :: (Eq a) => [a] -> [a] -> [Int]
-- subsequenceIndices [] _ = []
-- subsequenceIndices _ [] = error "`sub` was not a subsequence of `super`"
-- subsequenceIndices sub@(a:sub') super@(b:super') =
--     if a == b
--         then 0 : map succ (subsequenceIndices sub' super')
--         else     map succ (subsequenceIndices sub super')


-- | When `sub` is a (not necessarily contiguous) subsequence of `super`,
--   get the indices at which elements of `sub` do *not* appear. E.g.
--
--       > nonSubsequenceIndices "abe" "abcdefg"
--       > [2,3,5,6]
nonSubsequenceIndices :: (Eq a) => [a] -> [a] -> [Int]
nonSubsequenceIndices sub super =
    [0..(length super - 1)] \\ (subsequenceIndices sub super)


-- | /O(n)/. `indices` parameter *must* be sorted in increasing order,
--   and indices must all exist.
removeAtIndices :: [Int] -> [a] -> Either Error [a]
removeAtIndices dels list
    | not allValidIndices = Left "Fatal: couldn't apply list diff (application requires removing elements at invalid indices)."
    | otherwise = Right (removeAtIndices' 0 sortedDels list)
    where
        sortedDels = sort dels
        allValidIndices = all (< length list) sortedDels

        removeAtIndices' :: Int -> [Int] -> [a] -> [a]
        removeAtIndices' _ [] xs = xs
        removeAtIndices' curr (i:is) xs@(_:_)
            | curr < i = head xs : removeAtIndices' (succ curr) (i:is) (tail xs)
            | curr == i = removeAtIndices' (succ curr) is (tail xs)
            | otherwise = error "Indices must be sorted in ascending order."
        removeAtIndices' _ _ [] = [] -- This case should not occur due to `allValidIndices` check


-- | Don't hit the memo table if not necessary
-- | A wrapper around `longestCommonSubsequence`. It gives a bit of a
-- performance boost; it avoids hitting the memo table to an extent
-- (exactly how much depends on the arguments).
longestCommonSubsequenceWrapper :: forall a. (Eq a) => [a] -> [a] -> [a]
longestCommonSubsequenceWrapper xs ys =
    if xs == ys
        then xs -- (WLOG) don't want to return xs ++ xs
        else commonPrefix
            ++ longestCommonSubsequence (getMiddle xs) (getMiddle ys)
            ++ commonSuffix
    where
        commonPrefix :: [a]
        commonPrefix = getCommonPrefix xs ys

        -- drop (length commonPrefix) to prevent the "abc" vs. "abc*abc" case
        commonSuffix :: [a]
        commonSuffix = reverse
            (getCommonPrefix
                (reverse (drop (length commonPrefix) xs))
                (reverse (drop (length commonPrefix) ys)))

        getCommonPrefix :: [a] -> [a] -> [a]
        getCommonPrefix as bs = map fst . takeWhile (uncurry (==)) $ zip as bs

        -- xs = abcd***efg
        -- ys = abcd???????efg
        -- getMiddle xs == ****
        -- getMiddle ys = ??????
        getMiddle :: [a] -> [a]
        getMiddle elems = take (length elems - length commonPrefix - length commonSuffix) . drop (length commonPrefix) $ elems


-- | Compute the longest common (potentially noncontiguous) subsequence
--   between two sequences. Element type is fixed because memoization
--   requires a static type.
longestCommonSubsequence :: forall a. (Eq a) => [a] -> [a] -> [a]
longestCommonSubsequence xs ys = longestCommonSubsequence' 0 0
    where
        -- TODO: UArray?
        xs' :: HMap.Map Int a
        xs' = foldl update HMap.empty (zip [0..] xs)

        ys' :: HMap.Map Int a
        ys' = foldl update HMap.empty (zip [0..] ys)

        update :: HMap.Map Int a -> (Int, a) -> HMap.Map Int a
        update hmap (i, a) = HMap.insert i a hmap

        xsLength :: Int
        xsLength = length xs

        ysLength :: Int
        ysLength = length ys

        longestCommonSubsequence' :: Int -> Int -> [a]
        longestCommonSubsequence' = Memo.memo2 Memo.integral Memo.integral longestCommonSubsequence''

        longestCommonSubsequence'' :: Int -> Int -> [a]
        longestCommonSubsequence'' i j
            | i == xsLength = []
            | j == ysLength = []
            | x == y = x : longestCommonSubsequence' (i + 1) (j + 1) -- WLOG
            | length caseX > length caseY = caseX
            | otherwise = caseY
            where
                x :: a
                x = xs' HMap.! i

                y :: a
                y = ys' HMap.! j

                caseX :: [a]
                caseX = longestCommonSubsequence' (i + 1) j

                caseY :: [a]
                caseY = longestCommonSubsequence' i (j + 1)


-- | When `sub` is a (not necessarily contiguous) subsequence of `super`,
--   get the index at which each element of `sub` appears. E.g.
--
--       > subsequenceIndices "abe" "abcdefg"
--       > [0,1,4]
subsequenceIndices :: (Eq a) => [a] -> [a] -> [Int]
subsequenceIndices [] _ = []
subsequenceIndices _ [] = error "`sub` was not a subsequence of `super`"
subsequenceIndices sub@(a:sub') super@(b:super') =
    if a == b
        then 0 : map succ (subsequenceIndices sub' super')
        else     map succ (subsequenceIndices sub super')


-- | When `sub` is a (not necessarily contiguous) subsequence of `super`,
--   get the indices at which elements of `sub` do *not* appear. E.g.
--
--       > nonSubsequenceIndices "abe" "abcdefg"
--       > [2,3,5,6]
-- nonSubsequenceIndices :: (Eq a) => [a] -> [a] -> [Int]
-- nonSubsequenceIndices sub super =
--     [0..(length super - 1)] \\ (subsequenceIndices sub super)
