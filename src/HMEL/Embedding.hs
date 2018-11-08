{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, BangPatterns #-}

module HMEL.Embedding(Embedded(..), dist2, vectDist2) where

import qualified Data.Vector as Vector
import qualified Data.Map  as Map
import qualified Data.List as List
  

class Ord i => Embedded i v a | a -> i, a -> v where
  -- | vectorize x returns a list of features and values for 
  -- the embedding of x. The list must be sorted in the increasing 
  -- order of features.
  vectorize :: a -> [(i,v)]

instance Embedded Int a [a] where
  vectorize v = zip [0..] v

instance Embedded Int a (a,a) where
  vectorize (a,b) = [(0,a),(1,b)]

instance Embedded Int a (a,a,a) where
  vectorize (a,b,c) = [(0,a),(1,b),(2,c)]

instance Embedded Int a (a,a,a,a) where
  vectorize (a,b,c,d) = [(0,a),(1,b),(2,c),(3,d)]

instance Embedded Int a (a,a,a,a,a) where
  vectorize (a,b,c,d,e) = [(0,a),(1,b),(2,c),(3,d),(4,e)]

instance Embedded Int a (Vector.Vector a) where
  vectorize v = zip [0..] (Vector.toList v)

instance Ord i => Embedded i a (Map.Map i a) where
  vectorize = Map.toList

dist2 :: (Num v, Embedded i v a) => a -> a -> v
dist2 a b = vectDist2 0 (vectorize a) (vectorize b)

vectDist2 sum avec bvec =
  case (avec, bvec) of
    ((ia,va):avec, (ib,vb):bvec) -> common sum ia va avec ib vb bvec
    ((ia,va):avec, []          ) -> rest   sum ia va avec
    ([]          , (ib,vb):bvec) -> rest   sum ib vb bvec
    ([]          , []          ) -> sum
  where
    common !sum ia va avec ib vb bvec =
      case compare ia ib of
        LT -> let sum' = sum + diff2 va 0
              in case avec of
                   (ia,va):avec -> common sum' ia va avec ib vb bvec
                   []           -> rest   sum' ib vb bvec
        GT -> let sum' = sum + diff2 0 vb
              in case bvec of
                   (ib,vb):bvec -> common sum' ia va avec ib vb bvec
                   []           -> rest   sum' ia va avec
        EQ -> vectDist2 (sum + diff2 va vb) avec bvec

    rest !sum i v vec =
      let sum' = sum + diff2 v 0
      in case vec of
           (i,v):vec -> rest sum' i v vec
           []        -> sum'

    diff2 v1 v2 = (v1 - v2)^2
