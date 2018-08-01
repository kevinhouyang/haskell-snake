module Main where

import System.Random as R
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game as Game
import Data.Set as Set
import Data.Fixed


-- type/data declarations and constants --

type Snake = Path
type GameState = (Snake, Point, Int, Direction, Direction)
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

drawScore :: Int -> Picture
drawScore score = color white (text $ show score)

drawPicture :: GameState -> Picture
drawPicture (snake, apple, score, _, _) =  Pictures  ((drawScore score): (drawSquare red apple) : (drawSnake white snake))

drawSquare :: Color -> Point -> Picture
drawSquare c bottomLeft@(x, y) =
    let topLeft = (x, y + 1)
        topRight = (x + 1, y + 1)
        bottomRight = (x + 1, y)
        coords = (Prelude.map (\(x, y) -> ((x-10) * squareDim, (y-10) * squareDim)) [bottomLeft, topLeft, topRight, bottomRight])
    in color c (polygon coords)

drawSnake :: Color -> Snake -> [Picture]
drawSnake _ [] = []
drawSnake c snake@(x:xs) = (drawSquare c x) : drawSnake new_c xs
    where new_c = changeColor (rgbaOfColor c)

changeColor :: (Float, Float, Float, Float) -> Color
changeColor (a, b, c, d) = new_c
    where new_c = makeColor (a) (b - 0.1) (c - 0.1) (d)

-- functions for handling game logic --

-- generateApple :: Snake -> Point
-- generateApple snake
    -- | ((fromIntegral (fst randPoint)), (fromIntegral (snd randPoint))) `elem` snake = generateApple snake
    -- | otherwise = ((fromIntegral (fst randPoint)), (fromIntegral (snd randPoint)))
    -- where
        -- randPoint :: (Integer, Integer)
	    -- randPoint = (fst (randomR (-20, 20) (mkStdGen 0)), fst (randomR (-20, 20) (mkStdGen 0)))

-- generateApple :: Snake -> Point
-- generateApple snake
    -- | let
	--     randPoint :: (Integer, Integer)
	--     randPoint = (fst (randomR (-20, 20) (mkStdGen 0)), fst (randomR (-20, 20) (mkStdGen 0)))
	--   in (fromIntegral (fst randPoint), fromIntegral (snd randPoint)) `elem` snake = generateApple snake
    -- | let
	--     randPoint :: (Integer, Integer)
	--     randPoint = (fst (randomR (-20, 20) (mkStdGen 0)), fst (randomR (-20, 20) (mkStdGen 0)))
	--   in otherwise = (fromIntegral (fst randPoint), fromIntegral (snd randPoint))

-- generateApple :: Point
-- generateApple =
--     let x = randomR(-20, 20)
--         y = randomR(-20, 20)
--     in (x, y)

checkLoss :: Snake -> Bool
checkLoss snake = length snake /= length snake_set
    where snake_set = Set.fromList snake

moveSnake :: Snake -> Direction -> Snake
moveSnake snake (a, b) = (mod' (a + x) 20, mod' (b + y) 20) : init snake
    where (x, y) = head snake

growSnake :: Snake -> Direction -> Snake
growSnake snake (a, b) = (mod' (a + x) 20, mod' (b + y) 20) : snake
	where (x, y) = head snake

handleKeyEvent :: Game.Event -> GameState -> GameState
handleKeyEvent (EventKey k ks _ _) (snake, point, int, direction, curr_direction)
    | curr_direction /= (0, -1), SpecialKey KeyUp <- k, Down <- ks = (snake, point, int, (0, 1), curr_direction)
    | curr_direction /= (0, 1), SpecialKey KeyDown <- k, Down <- ks = (snake, point, int, (0, -1), curr_direction)
    | curr_direction /= (-1, 0), SpecialKey KeyRight <- k, Down <- ks = (snake, point, int, (1, 0), curr_direction)
    | curr_direction /= (1, 0), SpecialKey KeyLeft <- k, Down <- ks = (snake, point, int, (-1, 0), curr_direction)
    | otherwise = (snake, point, int, direction, curr_direction)
handleKeyEvent _ game = game

updateGame :: Float -> GameState -> GameState
updateGame _ (snake, apple, score, direction@(a, b), curr_direction)
    | checkLoss snake = (snake, apple, score, direction, curr_direction)
	| (a + x, b + y) == apple = (growSnake snake direction, apple, score + 1, direction, direction)
	| otherwise = (moveSnake snake direction, apple, score, direction, direction)
	where (x, y) = head snake

-- run the game! --

main :: IO ()
main =
    let newGame = (newSnake, (0,0), 0, (0, 1), (0, 1))
        keyFrame = 5
    in play window background keyFrame newGame drawPicture handleKeyEvent updateGame
	-- let
	    -- randPoint :: (Int, Int)
	    -- randPoint = (fst (randomR (-20, 20) (mkStdGen 0)), fst (randomR (-20, 20) (mkStdGen 0)))
    -- in putStr (show randPoint)

    -- display window background drawPicture
