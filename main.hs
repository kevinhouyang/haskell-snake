import Graphics.Gloss
import System.Random

type Coords = (Int, Int)
type Snake = [Coords]
data Direction = NORTH | SOUTH | EAST | WEST

-- window display
window :: Display
window = InWindow "Snake" (200, 200) (200, 200)

background :: Color
background = white

drawing :: Picture
drawing = blank

-- game logic
runGame :: 

moveSnake :: Snake -> Direction -> Snake
 
growSnake :: Snake -> Snake
growSnake = 

placeApple

updateScore :: Integer -> Integer

 

-- main function
main :: IO ()
main = display window background drawing
