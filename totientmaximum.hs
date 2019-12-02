

-- Find n such that (n / eulerTotient n) is maximal

totientRatios :: Int -> [Double]
totientRatios n = [fromIntegral a / fromIntegral (totient a) | a <- [1..n]]


--totient :: Int -> Int 
totient n = length (1:[j | j <- [2..(n-1)], coprime j n])

--coprime :: Int -> Int -> Bool
coprime a b 
  | a > b = coprime b a
  | b `mod` a == 0 = False
  | True  = (not . or) [a `mod` i == 0 && b `mod` i == 0 | i <- [2..a-1]]
