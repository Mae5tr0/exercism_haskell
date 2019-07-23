module School (School, add, empty, grade, sorted) where
import Student
import Grade
import Data.List

data School = School [Grade] deriving Show

add :: Int -> String -> School -> School
add gradeNum studentName school@(School grades) = case findGrade gradeNum school of
  Nothing  -> School $ (newGrade gradeNum studentName):grades
  Just x   -> School $ (addToGrade studentName x) : (filter (/= x) grades)

empty :: School
empty = School []

grade :: Int -> School -> [String]
grade gradeNum school = case findGrade gradeNum school  of
  Nothing -> []
  Just (Grade _ students)  -> studentsToList students

sorted :: School -> [(Int, [String])]
sorted (School grades) = map gradeToPair $ sort grades

findGrade :: Int -> School -> Maybe Grade
findGrade gradeNum (School grades)  = find (\(Grade currGrade _) -> currGrade == gradeNum) grades 
  
newGrade :: Int -> String -> Grade
newGrade gradeNum studentName = Grade gradeNum [Student studentName]

addToGrade :: String -> Grade -> Grade
addToGrade studentName (Grade gradeNum students) = Grade gradeNum $ (Student studentName):students
