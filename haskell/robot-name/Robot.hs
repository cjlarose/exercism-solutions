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
randomRobotName g = let (l1, g') = randomR ('A', 'Z') g
                        (l2, g'') = randomR ('A', 'Z') g'
                        (n1, g''') = randomR ('0', '9') g''
                        (n2, g'''') = randomR ('0', '9') g'''
                        (n3, g''''') = randomR ('0', '9') g'''' in
                        ([l1, l2, n1, n2, n3], g''''')
