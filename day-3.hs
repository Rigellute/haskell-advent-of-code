module FindSteps where

findRing :: Int -> Int
findRing target = head $ dropWhile (\x -> x * x < target) [1, 3..]

-- diff between target number and the square of the ring (bottom right hand corner of ring)
--
manhattenDistance :: Int -> Int
manhattenDistance 1 = 0
manhattenDistance target = ringSteps + offsetSteps
  where
    ring = findRing target
    ringSteps = (ring - 1) `div` 2
    diff = ring * ring - target
    offsetSteps = abs $ diff `mod` (ring - 1) - ringSteps
