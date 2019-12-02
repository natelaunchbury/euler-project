{- Module: Euler
   Description: Various Euler problems (projecteuler.net/archives) when my momentum is low 
   Maintainer: Nate Launchbury 2019 
-}

module Euler where

import Data.Char
import Data.List hiding (permutations)
import qualified Data.Set as S
import Data.Set(Set)
import Control.Applicative
import Library


primes = 2:[p | p<-[3,5..], isPrime p]
   where isPrime p = and [p `mod` n /= 0 | n<-[2..(p `div` 2)]]



-- Euler problem 20 
addDigits :: Integer -> Int
addDigits n = addDigitsH (show n) 
  where addDigitsH [] = 0
        addDigitsH (s:ss) = digitToInt s + addDigitsH ss


-- Euler problem 21
properDivisors :: Int -> [Int]
properDivisors n = [x | x <- [1..n `div` 2], n `rem` x == 0] 

amicableTo n = nub [a | a <- [2..n], b <- [a+1..n-1], 
                        a /= b, sum (properDivisors a) == sum (properDivisors b)]

amicableTo' :: Int -> Int -> [Int] -> [Int]
amicableTo' n m seen  
  | m==n = seen
  | True = let dm    = sum (properDivisors m) 
               new   = [b | b <- [1..m], b/=m, sum (properDivisors b) == dm]
               seen' = seen `union` (new \\ seen)
           in amicableTo' n (m+1) seen'

amicableTo'' :: Int -> Int -> Set Int -> Set Int
amicableTo'' n m seen
  | m==n = seen
  | True = let dm    = sum (properDivisors m) 
               new   = S.fromAscList [b | b <- [1..m], b/=m, sum (properDivisors b) == dm]
               seen' = seen `S.union` new
           in amicableTo'' n (m+1) seen'

-- Euler problem 22
e22main = do 
  input <- readFile "names.txt"
  let names = sort $ (lines . cleanup) input
  let points = map nameToPoints names
  let scores = zipWith (*) points [1..]
  putStrLn (show (sum scores))

-- |Turns a comma seperated string into a list (also removes quotes) 
cleanup :: String -> String
cleanup [] = [] 
cleanup ('"':ss) = cleanup ss
cleanup (',':ss) = '\n': cleanup ss
cleanup (s:ss)   = s : cleanup ss
  

nameToPoints :: String -> Int 
nameToPoints [] = 0 
nameToPoints (c:cs) = points c + nameToPoints cs

points :: Char -> Int
points c = ord c - 64


-- Euler problem 31

coins = [1,2,5,10,20,50,100,200]

--change :: Int -> [[Int]]
--change n = let rems = [n-a | a <- coins, n-a>=0]
--           in map (\x -> change x) rems 


-- Euler problem 42
triangleNumbers = scanl (+) 1 [2..]

-- |an 'elem' function which requires the list to be monotonic (and so works on infinite lists)
elemMonotonic :: Ord a => a -> [a] -> Bool
elemMonotonic n [] = False
elemMonotonic n (x:xs) 
  | x==n = True
  | x>n  = False
  | x<n  = elemMonotonic n xs

triangleWord :: String -> Bool
triangleWord s = let n = nameToPoints s in n `elemMonotonic` triangleNumbers

e42main = do 
  input <- readFile "words.txt"
  let words = (lines . cleanup) input 
  let ts = [w | w <- words, triangleWord w]
  putStrLn $ show (length ts) 


-- Euler problem 45
pentagonalNumbers = scanl (+) 1 [4,7..]

hexagonalNumbers = scanl (+) 1 [5,9..]

intersectTPH = [n | n <- [1..], n `elemMonotonic` triangleNumbers, 
                                n `elemMonotonic` pentagonalNumbers,
                                n `elemMonotonic` hexagonalNumbers]


-- Euler problem 46
goldbachFactors :: Int -> Either Int (Int,Int)
goldbachFactors n = 
  case [(a,b) | a <- take n primes, b <- take n [1..n], n == a + 2*b*b] of 
     []     -> Left n
     (x:xs) -> Right x
      
goldbachFalse = find isLeft [goldbachFactors n | n <- [5,7..], not (n `elemMonotonic` primes)]

-- ans: 5777
goldbachFalse' n
  | n `rem` 1000 < 10 = case goldbachFactors n of 
                           Left x -> do putStrLn $ "Done! " ++ show x  
                           y      -> do putStrLn $ show n ++ ": " ++ show y; 
                                        goldbachFalse' (nextOddComposite n)
  | otherwise         = case goldbachFactors n of 
                           Left x -> do putStrLn $ "Done! " ++ show x  
                           y      -> goldbachFalse' (nextOddComposite n) 
                          
nextOddComposite n = head [x | x <- [n+2,n+4..], not (x `elemMonotonic` primes)]

isLeft (Left _)  = True
isLeft (Right _) = False


-- Euler problem 54

e54main = do 
  input <- readFile "poker.txt"
  let hs = map (run parseHands) (lines input)
  let results = map (uncurry compareHands) hs
  let winner = filter (==GT) results
  putStrLn $ "Player 1 won " ++ show (length winner) ++ " hands out of " ++ show (length results)

data Suit = S | H | D | C 
  deriving (Eq,Show)

instance Ord Suit where
  compare _ _ = EQ

data Value = Two | Three | Four | Five | Six | Seven 
           | Eight | Nine | Ten | Jack | Queen | King | Ace 
  deriving (Eq,Ord,Show)

type Card = (Value, Suit)

eqCard :: Card -> Card -> Bool
eqCard (v1,s1) (v2,s2) = v1==v2

type Hand = [Card]

eqHand :: Hand -> Hand -> Bool
eqHand ps qs = and $ zipWith (eqCard) (sort ps) (sort qs)

value :: Card -> Value
value (v,s) = v

suit :: Card -> Suit
suit (v,s) = s

values :: Hand -> [Value]
values h = map value h

suits :: Hand -> [Suit]
suits h = map suit h


-- parsing
parseHands = do 
  v1 <- one; s1 <- one; _ <- space 
  v2 <- one; s2 <- one; _ <- space 
  v3 <- one; s3 <- one; _ <- space
  v4 <- one; s4 <- one; _ <- space
  v5 <- one; s5 <- one; _ <- space

  v6 <- one; s6 <- one; _ <- space
  v7 <- one; s7 <- one; _ <- space
  v8 <- one; s8 <- one; _ <- space
  v9 <- one; s9 <- one; _ <- space
  vt <- one; st <- one
  return (zip (map decodeVal [v1,v2,v3,v4,v5]) (map decodeSuit [s1,s2,s3,s4,s5]), 
          zip (map decodeVal [v6,v7,v8,v9,vt]) (map decodeSuit [s6,s7,s8,s9,st])) 


decodeVal :: Char -> Value
decodeVal '2' = Two
decodeVal '3' = Three
decodeVal '4' = Four
decodeVal '5' = Five
decodeVal '6' = Six
decodeVal '7' = Seven
decodeVal '8' = Eight
decodeVal '9' = Nine
decodeVal 'T' = Ten
decodeVal 'J' = Jack
decodeVal 'Q' = Queen
decodeVal 'K' = King
decodeVal 'A' = Ace

decodeSuit :: Char -> Suit
decodeSuit 'S' = S
decodeSuit 'H' = H
decodeSuit 'D' = D
decodeSuit 'C' = C


consecutive :: Hand -> Bool
consecutive [c] = True
consecutive ((Ace,_):(Five,_):(Four,_):(Three,_):(Two,_):[]) = True
consecutive ((Ace,_)  :(King,s) :cs) = consecutive ((King,s) :cs)
consecutive ((King,_) :(Queen,s):cs) = consecutive ((Queen,s):cs)
consecutive ((Queen,_):(Jack,s) :cs) = consecutive ((Jack,s) :cs)
consecutive ((Jack,_) :(Ten,s)  :cs) = consecutive ((Ten,s)  :cs)
consecutive ((Ten,_)  :(Nine,s) :cs) = consecutive ((Nine,s) :cs)
consecutive ((Nine,_) :(Eight,s):cs) = consecutive ((Eight,s):cs)
consecutive ((Eight,_):(Seven,s):cs) = consecutive ((Seven,s):cs)
consecutive ((Seven,_):(Six,s)  :cs) = consecutive ((Six,s)  :cs)
consecutive ((Six,_)  :(Five,s) :cs) = consecutive ((Five,s) :cs)
consecutive ((Five,_) :(Four,s) :cs) = consecutive ((Four,s) :cs)
consecutive ((Four,_) :(Three,s):cs) = consecutive ((Three,s):cs)
consecutive ((Three,_):(Two,s)  :cs) = consecutive ((Two,s)  :cs)
consecutive _ = False

sortDec :: Ord a => [a] -> [a]
sortDec xs = sortBy (flip compare) xs

frequencies :: Hand -> [(Value,Int)]
frequencies h = (nub . freqSort) $ map (\(c,s) -> (c,count c h)) h

freqSort :: [(Value,Int)] -> [(Value,Int)]
freqSort h = (reverse . sortOn snd . sortOn fst) h



count :: Value -> Hand -> Int
count v [] = 0 
count v ((c,s):cs)
  | v==c = 1 + count v cs
  | True = count v cs


-- Hand strength chart

data HandStrength = Hc | P2 | P22 | P3 | St | Fl | P32 | P4 | SF
  deriving (Eq,Ord)

instance Show HandStrength where
  show Hc  = "Highcard" 
  show P2  = "One pair"
  show P22 = "Two pairs" 
  show P3  = "Three-of-a-kind"
  show St  = "Straight"
  show Fl  = "Flush"
  show P32 = "Full house"
  show P4  = "Four-of-a-kind"
  show SF  = "Straight flush"

highcard :: Hand -> Value
highcard h = maximum (values h) 

onepair :: Hand -> Bool
onepair h = map snd (frequencies h) == [2,1,1,1]
            
twopair :: Hand -> Bool
twopair h = map snd (frequencies h) == [2,2,1]

threeofakind :: Hand -> Bool
threeofakind h = map snd (frequencies h) == [3,1,1]

straight :: Hand -> Bool
straight h = consecutive $ sortDec h

flush :: Hand -> Bool
flush h = length ((nub . suits) h) == 1

fullhouse :: Hand -> Bool
fullhouse h = map snd (frequencies h) == [3,2]

fourofakind :: Hand -> Bool
fourofakind h = map snd (frequencies h) == [4,1]

straightflush :: Hand -> Bool
straightflush h = straight h && flush h 

handStrength :: Hand -> HandStrength
handStrength h 
  | straightflush h = SF
  | fourofakind h   = P4
  | fullhouse h     = P32
  | flush h         = Fl
  | straight h      = St
  | threeofakind h  = P3
  | twopair h       = P22
  | onepair h       = P2
  | otherwise       = Hc


-- |Compares the highest card from each hand (to break hand strength ties)
higherCard :: Hand -> Hand -> Ordering
higherCard ps qs = compare (highcard ps) (highcard qs) 

-- |Used for breaking straight ties since A-10 should not tie with A-5
higherLowCard :: Hand -> Hand -> Ordering
higherLowCard ps qs = compare ((head . sort) ps) ((head . sort) qs) 

-- |Compares the value of the most powerful pair of each hand 
higherPaired :: Hand -> Hand -> Ordering
higherPaired ps qs = let fps = frequencies ps
                         fqs = frequencies qs
                     in compare ((fst . head) fps) ((fst . head) fqs)

-- |Special case for twopair, compares the second pairs 
higherSndPair :: Hand -> Hand -> Ordering
higherSndPair ps qs = let fps = frequencies ps
                          fqs = frequencies qs
                      in compare (fst (fps!!2)) (fst (fqs!!2))

-- |Compares the highest non-paired card in each hand 
higherKicker :: Hand -> Hand -> Ordering
higherKicker ps qs = let fps = frequencies ps
                         fqs = frequencies qs
                     in compare (head (filter (\(v,c) -> c==1) fps)) 
                                (head (filter (\(v,c) -> c==1) fqs))

-- |Compares the strength of two hands, based on normal poker rules
compareHands :: Hand -> Hand -> Ordering
compareHands ps qs = 
  let p = handStrength ps
      q = handStrength qs
  in case compare p q of 
        GT -> GT
        LT -> LT 
        EQ ->
         case p of 
           SF  -> higherLowCard ps qs
           P4  -> higherPaired ps qs 
           P32 -> higherPaired ps qs 
           Fl  -> higherCard ps qs 
           St  -> higherLowCard ps qs 
           P3  -> higherPaired ps qs 
           P22 -> case higherPaired ps qs of
                     GT -> GT
                     LT -> LT
                     EQ -> case higherSndPair ps qs of 
                               GT -> GT
                               LT -> LT 
                               EQ -> higherKicker ps qs 
           P2  -> case higherPaired ps qs of 
                     GT -> GT
                     LT -> LT
                     EQ -> higherKicker ps qs 
           Hc  -> higherCard ps qs  


exHand1 = [(Ace,S),(Nine,H),(Ace,H),(Queen,C),(Two,H)]
exHand2 = [(Ace,S),(Jack,S),(Ten,S),(Queen,S),(King,S)]
exHand3 = [(Ace,S),(Jack,S),(Jack,H),(Queen,S),(King,S)]
exHand4 = [(Ace,S),(Jack,S),(Jack,H),(Ace,H),(King,S)]


