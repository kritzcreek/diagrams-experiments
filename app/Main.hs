{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

main :: IO ()
main = someFunc

someFunc :: IO ()
someFunc = mainWith $ tournament 6

arrowOpts = with & gaps       .~ small
                 & headLength .~ local 0.15

tournament :: Int -> Diagram B
tournament n = atPoints (trailVertices $ regPoly n 1) (map node [1..n]) #
               applyAll [ connectOutside' arrowOpts j k | j <- [1..n-1], k <- [j+1..n]]

node :: Int -> Diagram B
node n = text (show n) # fontSizeL 0.2 # fc white <> circle 0.2 # fc green # named n

-- example :: Diagram B
-- example = circle 1 # fc blue
--                    # lw veryThick
--                    # lc purple
--                    # dashingG [0.2,0.05] 0

-- example2 :: Diagram B
-- example2 = square 1 # fc aqua === circle 1


-- circleSqV1 :: Diagram B
-- circleSqV1 = beside (r2 (1,1)) (circle 1) (square 2) # fc aqua

-- circleSqV2 :: Diagram B
-- circleSqV2 = beside (r2 (1,-2)) (circle 1) (square 2)

-- example3 :: Diagram B
-- example3 = hcat [circleSqV1, strutY 1, circleSqV2]

-- snuggling :: Diagram B
-- snuggling = ell # snugR <> ell # reflectY # snugL # showOrigin
--   where
--     ell :: Diagram B
--     ell = circle 1 # scaleX 0.5 # rotateBy (1/6)

-- circleSqT :: Diagram B
-- circleSqT   = square 1 `atop` circle 1 # translate (r2 (0.5, 0.3))

-- circleSqHT :: Diagram B
-- circleSqHT  = square 1 ||| circle 1 # translate (r2 (0.5, 0.3))

-- circleSqHT2 :: Diagram B
-- circleSqHT2 = square 1 ||| circle 1 # translate (r2 (19.5, 0.3))

-- example4 = hcat [circleSqT, strutX 0.5, circleSqHT]

-- example5 :: Diagram B
-- example5 = hrule (2 * sum sizes) === circles # centerX
--   where
--     circles :: Diagram B
--     circles = fc (green `mappend` blue) . hcat . map alignT . zipWith scale sizes
--               $ repeat (circle 1)
--     sizes   = [1..4] ++ reverse [1..3]


-- dick :: Diagram B
-- dick = triangle 1 (1 :: Integer)

-- shaftAndBalls :: Diagram B
-- shaftAndBalls = nut 0.5 (-1) <> rect 0.5 2 <> nut 0.5 1
--   where
--     nut :: Double -> Double -> Diagram B
--     nut r dir = circle r # translate (r2 (r * dir, -2 * r))
