module HMEL.TSNE(defaultTSNEOptions,TSNEOptions(..)
                ,tsne2D
                ) where

import System.Random
import Data.Random.Normal
import Data.List(transpose,zipWith4,inits)
import HMEL.Embedding

tsne2D :: (Random v,RealFloat v,Embedded i v a,RandomGen g) => TSNEOptions -> g -> [a] -> (v,[(v,v)],g)
tsne2D opts g input =
  let (st,g') = initState2D (length input) g
      st'     = iterate st
  in (cost pss st,stSolution st,g')
  where
    e   = realToFrac (tsneLearningRate opts)
    pss = neighbourProbabilities opts input

    iterate st
      | stIteration st > 100 = st
      | otherwise            = iterate (stepTSNE e pss st)

data TSNEOptions = TSNEOptions {
    tsnePerplexity   :: Int,
    tsneLearningRate :: Double
}

defaultTSNEOptions = TSNEOptions 30 10

data TSNEState v = TSNEState {
    stIteration :: Int,
    stSolution  :: [(v,v)],
    stGains     :: [(v,v)],
    stDeltas    :: [(v,v)]
}

initState2D :: (Floating v, Random v, RandomGen g) => Int -> g -> (TSNEState v,g)
initState2D n g =
  let (s,g1) = initSolution n g
  in (TSNEState 0 s (repeat (1,1)) (repeat (0,0)),g1)
  where
    rr = repeat.repeat

    initSolution n g
      | n == 0    = ([],g)
      | otherwise = let (x, g1) = normal' (0, 1e-4) g
                        (y, g2) = normal' (0, 1e-4) g1
                        (ps,g3) = initSolution (n-1) g2
                    in ((x, y):ps,g3)

stepTSNE :: (Ord v,Floating v) => v -> [[v]] -> TSNEState v -> TSNEState v
stepTSNE e ps st = TSNEState i' s' g' d'
  where
    i  = stIteration st
    s  = stSolution st
    g  = stGains st
    d  = stDeltas st
    gr = gradients ps st
    i' = i + 1
    s' = recenter (zipWith (\(x,y) (dx,dy) -> (x+dx,y+dy)) s d')
    g' = zipWith3 (\(gx,gy) (dx,dy) (grx,gry) -> (newGain gx dx grx,newGain gx dx grx)) g d gr
    d' = zipWith3 (\(gx,gy) (dx,dy) (grx,gry) -> (newDelta e i gx dx grx, newDelta e i gy dy gry)) g' d gr

-- newGain :: Gain -> Delta -> Gradient -> Gain
newGain g d gr =
  max 0.01 (if signum d == signum gr
              then g * 0.8
              else g + 0.2)

-- newDelta :: Double -> Int -> Gain -> Delta -> Gradient -> Delta
newDelta e i g' d gr = (m * d) - (e * g' * gr)
  where
    m = if i < 250 then 0.5 else 0.8

gradients :: (Ord v,Floating v) => [[v]] -> TSNEState v -> [(v, v)]
gradients pss st = zip (gradient (map fst ss)) (gradient (map snd ss))
  where
    gradient s = zipWith4 (f s) s pss qss qss'

    ss   = stSolution st
    i    = stIteration st
    qss  = qdist ss
    qss' = qdist' ss

    f s x ps qs qs' = sum $ zipWith4 g s ps qs qs'
      where
        g y p q q' = m * (x - y)
          where
            m = 4 * (k * p - q') * q
            k = if i < 100 then 4 else 1

cost :: (Ord v,Floating v) => [[v]] -> TSNEState v -> v
cost pss st = sumsum $ (zipWith.zipWith) c pss (qdist' (stSolution st))
  where
    c p q = -p * log q

qdist :: Floating v => [(v,v)] -> [[v]]
qdist ss = let ds = qd ss
           in zipWith (\ls rs -> ls++[0]++rs) (inits (head ds)) ds
  where
    qd []     = []
    qd (p:ps) = (map (q p) ps) : qd ps

    q (x1,y1) (x2,y2) = 1 / (1 + s)
      where
        s = (x1-x2) * (x1-x2) + (y1-y2) * (y1-y2)

qdist' :: (Ord v,Floating v) => [(v,v)] -> [[v]]
qdist' ss = (map.map) f qd
  where
    qd = qdist ss

    f q = max (q / sumsum qd) 1e-100

sumsum :: Num v => [[v]] -> v
sumsum m = sum (map sum m)

recenter :: Fractional v => [(v,v)] -> [(v,v)]
recenter ss = map r ss
  where 
    (xs,ys) = unzip ss
    len     = (realToFrac.length) ss
    r (x,y) = (mean xs-x,mean ys-y)

    mean vs = sum vs / len

neighbourProbabilities :: (RealFloat v,Embedded i v a) => TSNEOptions -> [a] -> [[v]]
neighbourProbabilities opts vs = symmetrize $ rawNeighbourProbabilities opts vs
  where
    symmetrize m = (zipWith.zipWith) f m (transpose m)
      where
        f x y = max a 1e-100
            where a = (x + y) / (2 * (realToFrac.length) m) 

rawNeighbourProbabilities :: (RealFloat v,Embedded i v a) => TSNEOptions -> [a] -> [[v]]
rawNeighbourProbabilities opts vs = map np vs
  where
    np   a = aps (beta a) vs a
    beta a = betaValue (binarySearchBeta opts vs a)

    aps beta bs a = map pj' bs
      where
        psum = sum $ map (pj beta a) bs

        pj' b = pj beta a b / psum

data Beta v = Beta {
    betaValue :: v,
    betaMin   :: v,
    betaMax   :: v
}

binarySearchBeta :: (RealFloat v,Embedded i v a) => TSNEOptions -> [a] -> a -> Beta v
binarySearchBeta opts vs = binarySearchBeta' opts vs 1e-4 0 (Beta 1 (-1/0) (1/0))

binarySearchBeta' :: (RealFloat v,Embedded i v a) => TSNEOptions -> [a] -> v -> Int -> Beta v -> a -> Beta v
binarySearchBeta' opts bs tol i beta a
  | i == 50            = beta
  | abs (e - t) < tol  = beta
  | e > t              = r $ incPrecision beta
  | otherwise          = r $ decPrecision beta
  where
    t = log (realToFrac (tsnePerplexity opts))
    e = entropyForInputValue (betaValue beta) bs a

    incPrecision (Beta b _ bmax) 
      | isInfinite bmax = Beta (b * 2) b bmax
      | otherwise       = Beta ((b + bmax) / 2) b bmax
    decPrecision (Beta b bmin _) 
      | isInfinite bmin = Beta (b / 2) bmin b
      | otherwise       = Beta ((b + bmin) / 2) bmin b

    r beta' = binarySearchBeta' opts bs tol (i+1) beta' a 

entropyForInputValue :: (Ord v,Floating v,Embedded i v a) => v -> [a] -> a -> v
entropyForInputValue beta bs a = sum $ map h bs
  where
    h b = if p > 1e-7 then -p * log p else 0
      where p = pj beta a b / psum

    psum = sum $ map (pj beta a) bs

pj beta a b
  | d == 0    = 0
  | otherwise = exp (-d * beta)
  where
    d = dist2 a b
