module LeapYear (isLeapYear) where

isLeapYear :: Integer -> Bool
isLeapYear year 
 | 0 == mod year 400 = True
 | 0 == mod year 100 = False
 | otherwise         = 0 == mod year 4 
