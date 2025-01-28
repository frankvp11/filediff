{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

-- | Data types used by `Filediff`
module Filediff.Types
(
 ListDiff(..)
 , Line
 , Error
) where

import GHC.Generics

import Data.Default
import Data.Function (on)

import qualified Data.Text as T

import Data.Maybe

import Zora.List (merge, merge_by)
import Data.List (find, intersect, intersectBy, sortBy, (\\))

import Data.Monoid
import Control.Applicative

-- | Diff between two lists. `dels` represents the indices
--   at which to delete, and `adds` represents the indices and
--   contents to add.
data ListDiff a = ListDiff
    { dels :: ![Int]
    , adds :: ![(Int, a)] }
    deriving (Show, Read, Eq, Generic)
  
instance Default (ListDiff a) where
    def :: ListDiff a
    def = ListDiff [] []

instance (Eq a, Ord a) => Semigroup (ListDiff a) where
  (<>) = mappend

instance (Eq a, Ord a) => Monoid (ListDiff a) where
    mempty :: ListDiff a
    mempty = ListDiff [] []
    
    mappend :: ListDiff a -> ListDiff a -> ListDiff a
    mappend
        (ListDiff abDels abAdds)
        (ListDiff bcDels bcAdds)
        = ListDiff acDels acAdds
        where
            acDels :: [Int]
            -- acDels = abDels ++ bDelsFromA
            acDels = merge abDels bDelsFromA

            -- Indices (in `a`) of elements that survive (a -> b)
            -- but not (b -> c)
            bDelsFromA :: [Int]
            bDelsFromA = mapMaybe getSurvivingDel bcDels

            getSurvivingDel :: Int -> Maybe Int
            getSurvivingDel bi =
                if bi `elem` (map snd survivingAIndicesInB)
                    then Just bi
                    else Nothing

            -- Indices (in b) of elements that survive (a -> b)
            -- (in format [(in a, in b)])
            survivingAIndicesInB :: [(Int, Int)]
            survivingAIndicesInB = indicesAfterAdds 0 survivingAIndices (map fst abAdds)

            -- Indices in `a` that are not deleted
            survivingAIndices :: [Int]
            survivingAIndices =
                if null abDels
                    then [0 .. maxIndexA]
                    else [0 .. maxIndexA] \\ abDels
              where
                maxIndexA = if null abDels then -1 else maximum abDels

            -- Given the starting index, a list of elements to keep, and
            -- a list of indices where additions occur, calculate their
            -- new indices after additions.
            indicesAfterAdds :: Int -> [Int] -> [Int] -> [(Int, Int)]
            indicesAfterAdds _ [] _ = []
            indicesAfterAdds currentIdx elems [] =
                zip [currentIdx ..] elems
            indicesAfterAdds currentIdx elems@(x:xs) (a:as)
                | currentIdx < a =
                    (currentIdx, x) : indicesAfterAdds (currentIdx + 1) xs (a:as)
                | otherwise =
                    indicesAfterAdds (currentIdx + 1) elems as

            acAdds :: [(Int, a)]
            acAdds = merge_by compareAddIndices bcAdds cAddsFromA

            compareAddIndices :: (Int, a) -> (Int, a) -> Ordering
            compareAddIndices = compare `on` fst

            cAddsFromA :: [(Int, a)]
            cAddsFromA = adjustAddIndices 0 (map snd survivingABAdds) (map fst bcAdds)

            -- Adds in (a -> b) that survive (b -> c)
            survivingABAdds :: [(Int, a)]
            survivingABAdds = survivingABAdds' abAdds bcDels

            survivingABAdds' :: [(Int, a)] -> [Int] -> [(Int, a)]
            survivingABAdds' [] _ = []
            survivingABAdds' adds [] = adds
            survivingABAdds' (a@(_, _) : adds) (d:dels) =
                case compare (fst a) d of
                    LT -> a : survivingABAdds' adds (d:dels)
                    EQ -> survivingABAdds' adds dels
                    GT -> survivingABAdds' (a : adds) dels

            -- Adjusts the indices for additions from `a` after considering `bcAdds`
            adjustAddIndices :: Int -> [a] -> [Int] -> [(Int, a)]
            adjustAddIndices _ [] _ = []
            adjustAddIndices i elems [] = zip [i..] elems
            adjustAddIndices i elems (a:as)
                | i < a =
                    (i, head elems) : adjustAddIndices (i + 1) (tail elems) (a:as)
                | otherwise =
                    adjustAddIndices (i + 1) elems as

-- | Data type for a line.
type Line = T.Text

-- | Basic error type.
type Error = String
