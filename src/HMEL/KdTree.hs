{-# LANGUAGE MultiParamTypeClasses #-}

module HMEL.KdTree(KdTree,fromList,toList,nearNeighbors) where

import Data.Maybe
import HMEL.Embedding

data KdTree i v p
  = KdNode {
      kdLeft  :: KdTree i v p,
      kdPoint :: p,
      kdRight :: KdTree i v p,
      kdAxis  :: i,   -- the index of the indexed axis
      kdCoord :: v    -- cached the coordinate of kdPoint on the axis
    }
  | KdEmpty


instance Functor (KdTree i v) where
  fmap _ KdEmpty                   = KdEmpty
  fmap f (KdNode l x r coord axis) = KdNode (fmap f l) (f x) (fmap f r) coord axis

instance Foldable (KdTree i v) where
  foldr f init KdEmpty          = init
  foldr f init (KdNode l x r _ _) = foldr f init3 l
    where init3 = f x init2
          init2 = foldr f init r

fromList :: (Ord v,Num v,Embedded i v a) => [a] -> KdTree i v a
fromList = fromPoints . map (\x -> let v = vectorize x in (v,v,x))
  where
    fromPoints [] = KdEmpty
    fromPoints ps = KdNode {
                      kdLeft  = fromPoints ls,
                      kdPoint = x,
                      kdRight = fromPoints rs,
                      kdAxis  = axis,
                      kdCoord = coord
                    }
      where
        (ls,axis,coord,(_,_,x),rs) = divide ps

    divide ps =
      case project Nothing [] [] ps of
        (Just i,ys) -> let Just (ls,(v,p),rs) = splitListOn (length ys `div` 2) fst ys
                       in (map snd ls,i,v,p,map snd rs)
        (Nothing,_) -> divide (restart ps)
      where
        project axis xs ys []                        = (axis,ys)
        project axis xs ys (p@(xvec,[],       x):ps) =
          project axis ((0,p):xs) ((0,p):ys) ps
        project axis xs ys (p@(xvec,(i,v):vec,x):ps) =
          case axis of
            Just j  -> case compare i j of
                         LT -> project (Just i) (p0:xs) ((v,p'):xs) ps
                         EQ -> project axis     (p0:xs) ((v,p'):ys) ps
                         GT -> project axis     (p0:xs) (p0    :ys) ps
            Nothing         -> project (Just i) (p0:xs) ((v,p'):ys) ps
          where
            p0 = (0,p)
            p' = (xvec,vec,x)

        restart = map (\(xvec,_,x) -> (xvec,xvec,x))

toList :: KdTree i v p -> [p]
toList = foldr (:) []

{-
-- | nearestNeighbor tree p returns the nearest neighbor of p in tree.
nearestNeighbor :: SparseVector i v p => KdTree i v p -> p -> Maybe p
nearestNeighbor KdEmpty                                    probe = Nothing
nearestNeighbor (KdNode KdEmpty pivot KdEmpty _         _) probe = Just pivot
nearestNeighbor (KdNode l       pivot r       xPivot axis) probe =
    if xProbe < xPivot then findNearest l r else findNearest r l
    where 
      xProbe = coord axis probe
      findNearest tree1 tree2 =
        let candidate1 = case nearestNeighbor tree1 probe of
                           Nothing   -> pivot
                           Just best -> List.minimumBy (compareDistance probe) [best, pivot]
            sphereIntersectsPlane = (xProbe - xPivot)^2 <= dist2 probe candidate1
            candidates2 = if sphereIntersectsPlane
                            then candidate1 : maybeToList (nearestNeighbor tree2 probe)
                            else [candidate1] 
        in Just . List.minimumBy (compareDistance probe) $ candidates2
-}

-- | nearNeighbors tree p returns all neighbors within distance r from p in tree.
nearNeighbors :: (Ord v, Num v, Embedded i v a) => KdTree i v a -> v -> a -> [a]
nearNeighbors t radius probe = search t xvec
  where
    xvec = vectorize probe

    search KdEmpty                             vec         = []
    search (KdNode KdEmpty x KdEmpty _    _)   vec         =
      [x | vectDist2 0 (vectorize x) xvec <= radius^2]
    search t                                   []          = search t xvec
    search t@(KdNode l       x r       axis v) ((i,v'):vec)=
      case compare i axis of
        LT -> if v' - abs radius <= 0 && v' + abs radius >= 0
                then search t vec
                else []
        EQ -> if v' <= v
                then let nearest = maybePivot ++ search l vec
                     in if v' + abs radius > v
                          then search r vec ++ nearest
                          else nearest
                else let nearest = maybePivot ++ search r vec
                     in if v' - abs radius < v
                          then search l vec ++ nearest
                          else nearest



        GT -> if 0 <= v
                then let nearest = maybePivot ++ search l vec
                     in if 0 + abs radius > v
                          then search r ((i,v'):vec) ++ nearest
                          else nearest
                else let nearest = maybePivot ++ search r vec
                     in if 0 - abs radius < v
                          then search l ((i,v'):vec) ++ nearest
                          else nearest
{-

        GT -> if - abs radius <= v && abs radius >= v
                then search t ((i,v'):vec)
                else []
-}
      where
        maybePivot = [x | vectDist2 0 (vectorize x) xvec <= radius^2]


{-
-- |kNearestNeighbors tree k p returns the k closest points to p within tree.
kNearestNeighbors :: (Eq p, Point p) => KdTree p -> Int -> p -> [p]
kNearestNeighbors KdEmpty _ _ = []
kNearestNeighbors _ k _ | k <= 0 = []
kNearestNeighbors tree k probe = nearest : kNearestNeighbors tree' (k-1) probe
    where nearest = fromJust $ nearestNeighbor tree probe
          tree' = tree `remove` nearest

-- |remove t p removes the point p from t.
remove :: (Eq p, Point p) => KdTree p -> p -> KdTree p
remove KdEmpty _ = KdEmpty
remove (KdNode l p r axis) pKill
  | p == pKill = fromListWithDepth (toList l ++ toList r) axis
  | coord axis pKill <= coord axis p = KdNode (remove l pKill) p r axis
  | otherwise = KdNode l p (remove r pKill) axis

-}

-- | Split the list around the k-th smallest value
splitListOn :: Ord b => Int -> (a -> b) -> [a] -> Maybe ([a],a,[a])
splitListOn k f = split 0 k [] []
  where
    split i k ls rs []     = Nothing
    split i k ls rs (x:xs) =
      partition i (i+1) k ls [x] rs xs

    partition i j k ls ms@(m:ms') rs []
      | i > k     = split 0 k [] (ms++rs) ls
      | k > j     = split j k (ms++ls) [] rs
      | otherwise = Just (ls,m,ms'++rs)
    partition i j k ls ms@(m:_) rs (x:xs) =
      case compare (f m) (f x) of
        LT -> partition  i     j    k    ls    ms (x:rs) xs
        EQ -> partition  i    (j+1) k    ls (x:ms)   rs  xs
        GT -> partition (i+1) (j+1) k (x:ls)   ms    rs  xs

