module Robot (robotName, mkRobot, resetName) where

import System.Random (RandomGen, randomR, getStdRandom)
import Data.IORef (newIORef, writeIORef, readIORef)

mkRobot = do
  name <- getStdRandom randomRobotName
  newIORef name

resetName r = do
  name <- getStdRandom randomRobotName
  writeIORef r name

robotName = readIORef

randomRobotName :: RandomGen g => g -> (String, g)
randomRobotName g = foldr f ([], g) pat
  where
    pat = [('A', 'Z'), ('A', 'Z'), ('0', '9'), ('0', '9'), ('0', '9')]
    f r (res, gen) = let (x, gen') = randomR r gen in
      (x : res, gen')
