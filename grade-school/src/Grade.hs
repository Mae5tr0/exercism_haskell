module Grade where
import Data.List             
import Student

data Grade = Grade Int [Student] deriving Show

instance Ord Grade where
  (Grade g1 _) `compare` (Grade g2 _) = g1 `compare` g2

instance Eq Grade where
 (Grade g1 _) == (Grade g2 _) = g1 == g2

gradeToPair :: Grade -> (Int,[String])
gradeToPair g@(Grade grade xs) = (grade, studentsToList xs)

studentsToList :: [Student] -> [String]
studentsToList = map (\(Student name) -> name) . sort

