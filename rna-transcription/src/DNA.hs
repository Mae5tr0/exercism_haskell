module DNA (toRNA) where

toRNA :: String -> Either Char String
toRNA xs = mapM complement xs
  where 
    complement 'C' = Right 'G'
    complement 'G' = Right 'C'
    complement 'T' = Right 'A'
    complement 'A' = Right 'U'
    complement x = Left x


