module Acronym (abbreviate) where

import Data.Char

abbreviate :: String -> String 
abbreviate = foldr (\word result -> abbr word ++ result) "" . wordsWhen (\x -> isSpace x || x == '-')

abbr :: String -> String
abbr [] = []
abbr xss@(x:_)
  | all isUpper xss  = [x]
  | length upperLetters > 1 = upperLetters
  | otherwise        = [(toUpper x)]
  where upperLetters = filter isUpper xss


wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'
