
import Library
import Control.Applicative
import Data.List
import Data.Char
import Data.Bits

{- 
 The file encryption.txt contains a list of ASCII values representing a ciphertext encrypted using cyclic XOR with a 3-character (lowercase letters) key. The goal is to decrypt the ciphertext under the assumption that the plaintext will contain common English words. 
 -}

main = do 
  input <- readFile "encryption.txt"
  let ciphertext = run parseCiphertext input
  let candidates = filter (\c -> score c >= 3) (decryptions ciphertext) 
  let finalist = (unwords . last) $ sortOn score candidates
  putStrLn finalist
  return (sum $ map ord finalist) 


parseCiphertext = do 
  n  <- num
  ns <- many nextInt
  return (n:ns) 


 
commonWords = ["a", "the", "and", "to", "is", "hello", "or", "I", "you", "of", "for", "by"]


passwords = [a:b:c:[] | a <- ['a'..'z'], b <- ['a'..'z'], c <- ['a'..'z']]


encrypt :: String -> [Int] -> [Int]
encrypt []     _      = []
encrypt (s:ss) (k:ks) = (xor (ord s) k) : encrypt ss (ks++[k])


decrypt :: [Int] -> [Int] -> String
decrypt []     _      = []
decrypt (c:cs) (k:ks) = chr (xor c k) : decrypt cs (ks++[k])


decryptions c = [words (decrypt c (map ord p)) | p <- passwords]


score :: [String] -> Int
score [] = 0
score (s:ss) 
  | s `elem` commonWords = 1 + score ss
  | otherwise = score ss 







