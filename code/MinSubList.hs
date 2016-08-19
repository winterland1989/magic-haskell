module MinSubList where

minSubList :: (Num a, Ord a) => [a] -> Int -> a
minSubList xs m = initSum + minDiff
  where
    shifted = drop m xs
    initSum = sum $ take m xs
    minDiff = minimum $ scanl (+) 0 $ zipWith (-) shifted xs

main = print $ minSubList  [2, 6, 4, 2, 5, 8, 3, 1] 3
