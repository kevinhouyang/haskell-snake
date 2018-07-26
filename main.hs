module Main where

import System.Random
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game as Game


-- type/data declarations and constants --

type Snake = Path
type GameState = (Snake, Point, Int, Direction)

-- data Direction = NORTH | SOUTH | EAST | WEST deriving (Enum, Eq)
type Direction = Point

newSnake :: Snake
newSnake = [(0, 0), (1, 0), (2, 0)]

squareDim :: Float
squareDim = 30

windowDim :: Int
windowDim = 600


-- functions for setting the game window --

window :: Display
window = InWindow "Snake" (windowDim, windowDim) (100, 100)

background :: Color
background = black


-- functions for rendering the picture --

drawPicture :: GameState -> Picture
drawPicture (snake, apple, _, _) =  Pictures  (color red (drawSquare apple) : (drawSnake snake))

drawSquare :: Point -> Picture
drawSquare bottomLeft@(x, y) =
    let topLeft = (x + 1, y)
        topRight = (x + 1, y - 1)
        bottomRight = (x, y - 1)
    in polygon (map (\(x, y) -> (x * squareDim, y * squareDim)) [bottomLeft, topLeft, topRight, bottomRight])

drawSnake :: Snake -> [Picture]
drawSnake [] = []
drawSnake snake@(x:xs) = color white (drawSquare x) : drawSnake xs

-- functions for handling game logic --

generateApple :: Snake -> Point
generateApple snake
	| randPoint `elem` snake = generateApple snake
	| otherwise = randPoint
	-- to DOOOO!!! figure out how randomR workss...
	where randPoint = (fst (randomR (-20, 20) (mkStdGen 6)), fst (randomR (-20, 20) (mkStdGen 7)))

--moveSnake :: Snake -> Direction -> Snake
--moveSnake snake direction
--    | direction == NORTH = (x, y + 1): init snake
--    | direction == SOUTH = (x, y - 1): init snake
--    | direction == WEST = (x - 1, y): init snake
--    | direction == EAST = (x + 1, y): init snake
--    where (x,y) = head snake

moveSnake :: Snake -> Direction -> Snake
moveSnake snake (a, b) = (a + x, b + y) : init snake
    where (x, y) = head snake

--growSnake :: Snake -> Direction -> Snake
--growSnake snake direction
--	  | direction == NORTH = (x, y + 1): snake
--    | direction == SOUTH = (x, y - 1): snake
--    | direction == WEST = (x - 1, y): snake
--    | direction == EAST = (x + 1, y): snake
--	where (x,y) = head snake
	
growSnake :: Snake -> Direction -> Snake
growSnake snake (a, b) = (a + x, b + y) : snake
	where (x, y) = head snake
	
--updateGame :: Float -> GameState -> GameState
--updateGame _ (snake, apple@(x, y), score, direction)
-- snake eats the apple
--	  | direction == NORTH && head snake == (x, y - 1) = (growSnake snake direction, generateApple (growSnake snake direction), score, direction)
--	  | direction == SOUTH && head snake == (x, y + 1) = (growSnake snake direction, generateApple (growSnake snake direction), score, direction)
--    | direction == WEST && head snake == (x + 1, y) = (growSnake snake direction, generateApple (growSnake snake direction), score, direction)
--    | direction == EAST && head snake == (x - 1, y) = (growSnake snake direction, generateApple (growSnake snake direction), score, direction)
--	  | otherwise = (moveSnake snake direction, apple, score, direction)
	
updateGame :: Float -> GameState -> GameState
updateGame _ (snake, apple, score, direction@(a, b))
-- snake eats the apple
	| (a + x, b + y) == apple = (growSnake snake direction, generateApple (growSnake snake direction), score, direction)
	| otherwise = (moveSnake snake direction, apple, score, direction)
	where (x, y) = head snake

handleKeyEvent :: Game.Event -> GameState -> GameState
handleKeyEvent (EventKey k ks _ _) (snake, point, int, direction)
    | SpecialKey KeyUp <- k, Down <- ks = (snake, point, int, (0, 1))
    | SpecialKey KeyDown <- k, Down <- ks = (snake, point, int, (0, -1))
    | SpecialKey KeyRight <- k, Down <- ks = (snake, point, int, (1, 0))
    | SpecialKey KeyLeft <- k, Down <- ks = (snake, point, int, (-1, 0))
    | otherwise = (snake, point, int, direction)
handleKeyEvent _ game = game

--updateGame :: Float -> GameState -> GameState
--updateGame _ (snake, apple, score, direction) = (newSnake, apple, score, direction)
--    where newSnake = updateSnake snake direction




-- run the game! --

main :: IO ()
main =
    let newGame = (newSnake, (-1,-1), 0, (0, 1))
        keyFrame = 4
    in play window background keyFrame newGame drawPicture handleKeyEvent updateGame

-- display window background drawPicture
