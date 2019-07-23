module Robot
    ( Bearing(East,North,South,West)
    , bearing
    , coordinates
    , mkRobot
    , move
    ) where

data Bearing = North
             | East
             | South
             | West
             deriving (Eq, Show, Enum)

data Robot = Robot Bearing (Integer, Integer)

bearing :: Robot -> Bearing
bearing (Robot dir _) = dir

coordinates :: Robot -> (Integer, Integer)
coordinates (Robot _ coord) = coord

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot = Robot

move :: Robot -> String -> Robot
move robot commands = foldl processCommand robot commands 
  where 
    processCommand robot' 'L' = turnLeft robot'
    processCommand robot' 'R' = turnRight robot'
    processCommand robot' 'A' = advance robot'
    --unknow action
    processCommand robot' _ = robot'

turnLeft :: Robot -> Robot
turnLeft (Robot North coord) = Robot West coord
turnLeft (Robot dir coord) = Robot (pred dir) coord

turnRight :: Robot -> Robot
turnRight (Robot West coord) = Robot North coord
turnRight (Robot dir coord) = Robot (succ dir) coord

advance :: Robot -> Robot
advance (Robot North (x,y)) = Robot North (x, succ y)
advance (Robot East (x,y)) = Robot East (succ x, y)
advance (Robot South (x,y)) = Robot South (x, pred y)
advance (Robot West (x,y)) = Robot West (pred x, y)
