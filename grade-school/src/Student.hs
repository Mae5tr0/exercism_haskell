module Student where

data Student = Student String deriving Show

instance Ord Student where
  (Student x) `compare` (Student y) = x `compare` y

instance Eq Student where
 (Student x) == (Student y) = x == y




