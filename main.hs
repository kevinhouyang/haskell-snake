module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game as Game


-- type/data declarations and constants --

type Snake = Path
type GameState = (Snake, Point, Int, Direction)

data Direction = NORTH | SOUTH | EAST | WEST deriving (Enum, Eq)

newSnake :: Snake
newSnake = [(0, 0), (1, 0), (2, 0)]

squareDim :: Float
squareDim = 30

windowDim :: Int
windowDim = 500


-- functions for setting the game window --

window :: Display
window = InWindow "Snake" (windowDim, windowDim) (100, 100)

background :: Color
background = black


-- functions for rendering the picture --

drawPicture :: GameState -> Picture
drawPicture (snake, _, _, _) = Pictures (drawSnake snake)

drawSquare :: Point -> Picture
drawSquare bottomLeft@(x, y) =
    let topLeft = (x + 1, y)
        topRight = (x + 1, y - 1)
        bottomRight = (x, y - 1)
    in color white (polygon (map (\(x, y) -> (x * squareDim, y * squareDim)) [bottomLeft, topLeft, topRight, bottomRight]))

drawSnake :: Snake -> [Picture]
drawSnake snake | length snake == 1 = (drawSquare (head snake)) : []
                | otherwise = (drawSquare (head snake)) : (drawSnake (tail snake))


-- functions for handling game logic --

handleKeyEvent :: Game.Event -> GameState -> GameState
handleKeyEvent (EventKey k ks _ _) (snake, point, int, direction)
    | SpecialKey KeyUp <- k, Down <- ks = (snake, point, int, NORTH)
    | SpecialKey KeyDown <- k, Down <- ks = (snake, point, int, SOUTH)
    | SpecialKey KeyRight <- k, Down <- ks = (snake, point, int, EAST)
    | SpecialKey KeyLeft <- k, Down <- ks = (snake, point, int, WEST)
    | otherwise = (snake, point, int, direction)
handleKeyEvent _ game = game

updateGame :: Float -> GameState -> GameState
updateGame _ (snake, apple, score, direction) = (newSnake, apple, score, direction)
    where newSnake = updateSnake snake direction

updateSnake :: Snake -> Direction -> Snake
updateSnake snake direction
    | direction == NORTH = (x, y + 1): init snake
    | direction == SOUTH = (x, y - 1): init snake
    | direction == WEST = (x - 1, y): init snake
    | direction == EAST = (x + 1, y): init snake
    where (x,y) = head snake


-- run the game! --

main :: IO ()
main =
    let newGame = (newSnake, (200,200), 0, NORTH)
        keyFrame = 2
    in play window background keyFrame newGame drawPicture handleKeyEvent updateGame

-- display window background drawPicture
