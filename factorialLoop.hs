

{-
 Problem is to find the total number of numbers who's factorial loop chain gets stuck at exactly 60 iterations. 
 A factorial loop is the sum of the factorial of the digits of a number (`factorialLoop`). 
 Note that every permutation of a number will have the same `stuck` number. This is not taken into account 
 in this solution. 
 
 Ans: 402 
 -}
problem74 = [x | x <- [1..1000000], fst (stuck x) == 60]


stuck n = stuckH n 1 [n] 

stuckH :: Int -> Int -> [Int] -> (Int,[Int])
stuckH n i seen 
  | next `elem` seen = (i,reverse (next:seen))
  | otherwise        = stuckH next (i+1) (next:seen)
  where next = factorialLoop n 

factorial 0 = 1 
factorial n = foldl (*) 1 [1..n]

factorialLoop n = sum (map factorial (digits n))

digits :: Int -> [Int]
digits n = map toInt (show n) 

toInt :: Char -> Int
toInt '0' = 0 
toInt '1' = 1 
toInt '2' = 2 
toInt '3' = 3 
toInt '4' = 4 
toInt '5' = 5 
toInt '6' = 6 
toInt '7' = 7 
toInt '8' = 8 
toInt '9' = 9 




