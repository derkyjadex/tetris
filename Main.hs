{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Map as Map
import System.IO
import Control.Concurrent (threadDelay)

data Color = Red | Green | Yellow | Blue
type Coord = (Int, Int)
type Board = Map.Map Coord Color
type Shape = [Coord]
data Piece = Piece Shape Coord Color
data Game = Game Board Piece

-- 10 x 30
line, ell, jay, square, zed, ess, tee :: Shape

line   = [(0,0), (1,0), (2,0), (3,0)]
ell    = [(0,1), (0,0), (1,0), (2,0)]
jay    = [(2,1), (0,0), (1,0), (2,0)]
square = [(1,1), (1,0), (0,1), (0,0)]
zed    = [(0,1), (1,1), (1,0), (2,0)]
ess    = [(0,0), (1,1), (1,0), (2,1)]
tee    = [(0,0), (1,0), (2,0), (1,1)]

aPiece :: Piece
aPiece = Piece ell (6,10) Red

aBoard :: Board
aBoard = foldr f Map.empty [(5,3,Green), (5,6,Yellow), (5,5,Blue)]
    where f (x,y,c) = Map.insert (x,y) c

aGame :: Game
aGame = Game aBoard aPiece

getLineStr :: Board -> Int -> String
getLineStr board y = "║" ++ (concat $ map f [0..9]) ++ "\ESC[40m║"
    where f x = case Map.lookup (x, y) board of
                Just Red -> "\ESC[41mλ="
                Just Green -> "\ESC[42mλ="
                Just Yellow -> "\ESC[43mλ="
                Just Blue -> "\ESC[44mλ="
                Nothing -> "\ESC[40m  "

getBoardStr :: Board -> String
getBoardStr board = unlines $ ["\ESC[H"] ++ [top] ++ main ++ [bottom]
    where
        top = "╔" ++ (replicate 20 '═') ++ "╗"
        main = map (getLineStr board) [30,29..0]
        bottom = "╚" ++ (replicate 20 '═') ++ "╝"

getGameStr :: Game -> String
getGameStr (Game b p) = getBoardStr $ merge b p

translate :: Piece -> [Coord]
translate (Piece s origin _) = map (addCoords origin) s

addCoords :: Coord -> Coord -> Coord
addCoords (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

merge :: Board -> Piece -> Board
merge b p@(Piece _ _ c) = foldr (\coord board -> Map.insert coord c board) b worldPiece
  where worldPiece = translate p

conflicts :: Board -> Piece -> Bool
conflicts b p = any hit worldPiece
  where worldPiece = translate p
        hit c = Map.member c b

move :: Coord -> Piece -> Piece
move c (Piece s origin color) = Piece s (addCoords origin c) color

down :: Piece -> Piece
down = move (0, -1)

left :: Piece -> Piece
left = move (-1, 0)

right :: Piece -> Piece
right = move (1, 0)

rotateLeft :: Piece -> Piece
rotateLeft = rotateRight . rotateRight . rotateRight

rotateRight :: Piece -> Piece
rotateRight (Piece cs o c) = Piece cs' o c
  where cs' = map f cs
        f (x, y) = (y, -x)

command :: Char -> Piece -> Piece
command 's' = down
command 'a' = left
command 'd' = right
command 'q' = rotateLeft
command 'e' = rotateRight
command _ = id

step :: (Piece -> Piece) -> Game -> Game
step f (Game b p) = Game b (f p)

getAvailChars :: IO String
getAvailChars = do
    chars <- readChars []
    return $ reverse chars
    where readChars prev = do
          ready <- hReady stdin
          if ready
              then do
                  c <- hGetChar stdin
                  readChars (c : prev)
              else return prev

getCommands :: IO [Piece -> Piece]
getCommands = do
    chars <- getAvailChars
    return $ map command chars

run :: Game -> IO Game
run game = do
    putStr $ getGameStr game
    threadDelay 100000
    commands <- getCommands
    let game' = foldr step game commands
    run game'

main :: IO Game
main = do
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    putStr "\ESC[2J"
    run (Game aBoard aPiece)

