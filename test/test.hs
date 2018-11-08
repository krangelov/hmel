{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import HMEL.KdTree
import HMEL.Embedding
import Graphics.UI.GIO
import Data.IORef
import System.Random
import Data.Random.Normal

instance Embedded Int Int Point where
  vectorize p = [(0,px p),(1,py p)]

-- Start of the program.
main :: IO ()
main = start "KdTreeTest" "1.0" SDI [] $ do
  g   <- newStdGen

  let vsize = sz 1500 900

  let ps = uniformRandomPoints (0,sw vsize) (0,sh vsize) g
      --ps = normalRandomPoints  (fromIntegral (sw vsize)/2,fromIntegral (sw vsize)/8) (fromIntegral (sh vsize)/2,fromIntegral (sh vsize)/8) g
  ref <- newIORef (Nothing, 200, fromList ps)

  -- Main window
  w <- window [ view       =: vsize
              , on paint   =: onPaint ref
              , on dismiss =: halt
              , resizeable =: False
              ]
  set w [on click   =: onClick w ref]
  showWindow w


uniformRandomPoints xrange yrange g = take 10000 (randomPoints g)
  where
    randomPoints g =
      let (x,g1) = randomR xrange g
          (y,g2) = randomR yrange g1
      in (pt x y):randomPoints g2

normalRandomPoints xparam yparam g = take 10000 (randomPoints g)
  where
    randomPoints g =
      let (x,g1) = normal' xparam g
          (y,g2) = normal' yparam g1
      in (pt (round (x :: Double)) (round (y :: Double))):randomPoints g2

onPaint ref can _ _ = do
  (mb_sel,radius,points) <- readIORef ref
  mapM_ (\p -> fillCircle p 3 can) points
  case mb_sel of
    Just (center,sel) -> do drawCircle center radius can
                            setCanvasPen can [color =: lightgreen]
                            mapM_ (\p -> fillCircle p 3 can) sel
    Nothing           -> return ()

onClick w ref p =  do
  (sel,radius,points) <- readIORef ref
  writeIORef ref (Just (p,nearNeighbors points radius p),radius,points)
  repaint w
