module RunLength (decode, encode) where

import Data.Char (isDigit)
import Data.List (group)

decode :: String -> String
decode [] = [] 
decode encodedText = replicate n sym ++ decode rest
  where
    (symLength, sym:rest) = span isDigit encodedText
    n = if null symLength then 1 else read symLength

encode :: String -> String
encode = concatMap encode' . group
  where 
    encode' [x] = [x]
    encode' xss@(x:_) = show (length xss) ++ [x]

{-- First attemp through foldl
decode :: String -> String
decode [] = ""
decode encodedText = fst $ foldl decode' ("", "") encodedText

decode' :: (String, String) -> Char -> (String, String)
decode' (result, counter) y 
 | isDigit y = (result, counter ++ [y]) 
 | otherwise = (result ++ generateString y counter, "")

generateString :: Char -> String -> String
generateString symbol "" = [symbol]
generateString symbol counter = replicate (read counter) symbol

encode :: String -> String
encode [] = ""
encode (x:xs) = buildResult $ foldl encode' ("", x, 1) xs 

encode' :: (String, Char, Integer) -> Char -> (String, Char, Integer)
encode' acc@(result, curr, counter) y
  | y == curr = (result, curr, counter + 1)  
  | otherwise = (buildResult acc, y, 1) 
    
buildResult :: (String, Char, Integer) -> String
buildResult (result, curr, counter)
 | counter == 1 = result ++ [curr]
 | otherwise    = result ++ (show counter) ++ [curr]
--}
