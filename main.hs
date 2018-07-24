module Main where

import Graphics.Gloss

import Graphics.Gloss.Interface.Pure.Game as Game

type Snake = [Point]

-- Instantiate Initial Variables

squareDim :: Float
squareDim = 30

startCoords :: Point
startCoords = (50, 50)

window :: Display
window = InWindow "Snake" (200, 200) (100, 100)

background :: Color
background = black

drawing :: Picture
drawing = blank

world :: Path
world = [(-40, 50), (0, 0)]

convertWorld :: Path -> Picture
convertWorld x = line x

--dummyUpdate :: Game.Event -> world -> world
--dummyUpdate event world = 

--updateWorld :: Float -> Path -> Path
--updateWorld num world = zip (map succ (fst (unzip world))) (snd (unzip world))

-- Initial Functions 

drawSquare :: Point -> Picture
drawSquare upperLeft = color white (polygon [upperLeft, ((fst upperLeft) + squareDim, snd upperLeft), ((fst upperLeft) + squareDim, (snd upperLeft) + squareDim), (fst upperLeft, (snd upperLeft) + squareDim)])

drawSnake :: Path -> [Picture]
drawSnake snake | length snake == 1 = (drawSquare (head snake)) : []
                | otherwise = (drawSquare (head snake)) : (drawSnake (tail snake))

point :: Point
point = (50,50)

square :: Picture
square = drawSquare point

testSnake :: Path
testSnake = [(0, 0), (40, 0), (80, 0)]

snakePicture :: Picture
snakePicture = Pictures (drawSnake testSnake)

main :: IO ()
main = display window background snakePicture

-- play window background 1 world convertWorld dummyUpdate updateWorld
