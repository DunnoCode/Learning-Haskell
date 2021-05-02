module TicTacToe (tictactoe) where

import Control.Applicative
import Control.Monad
import Control.Monad.State

import Data.Char
import Data.List

import Text.Printf


-- | Problem 1.

data Move = X | O
data Cell = Occuppied Move | Empty


instance Show Move where
    show X = "X"
    show O = "O"

instance Show Cell where
    show (Occuppied X) = " X "
    show (Occuppied O) = " O "
    show Empty         = "   "

instance Eq Cell where
    Occuppied X == Occuppied X = True
    Occuppied O == Occuppied O = True
    Empty == Empty = True
    _ == _ = False

nextMove :: Move -> Move
nextMove X = O
nextMove O = X

printRow :: [Cell] -> String
printRow row = "|" ++ (intercalate "|" $ fmap show row) ++ "|\n" 

printCover :: String
printCover = ".---.---.---.\n"

printBoard :: [Cell] -> IO ()
printBoard board = do putStr printCover
                      putStr (printRow (take 3 board))
                      putStr printCover
                      putStr (printRow (take 3 (drop 3 board)))
                      putStr printCover
                      putStr (printRow (take 3 (drop 6 board)))
                      putStr printCover

checkEmpty :: Int -> [Cell] -> Bool
checkEmpty index board 
    | returnFalse = False
    | returnTrue = True
    | otherwise = False
    where 
        returnFalse = index == -1
        returnTrue = board !! index == Empty

removeSpace :: [Char] -> [Char]
removeSpace [] =  []
removeSpace (' ': cs) = removeSpace cs
removeSpace (c:cs) = c : removeSpace cs

inputConverter :: String -> Int
inputConverter input = case (length(removeSpace input) == 2) of
                            True -> case (isDigit ((removeSpace input) !! 0) && isDigit ((removeSpace input) !! 1)) of
                                        True -> case ((digitToInt ((removeSpace input) !! 0) < 4) && (digitToInt ((removeSpace input) !! 1) < 4)) of
                                                    True -> case (digitToInt ((removeSpace input) !! 0) - 1)*3 + (digitToInt ((removeSpace input) !! 1) - 1) of 
                                                                0 -> 0
                                                                1 -> 1
                                                                2 -> 2
                                                                3 -> 3
                                                                4 -> 4
                                                                5 -> 5
                                                                6 -> 6
                                                                7 -> 7
                                                                8 -> 8
                                                                _ -> -1
                                                    False -> -1
                                        False -> -1
                            False -> -1

inputCell :: Int -> Move -> [Cell] -> [Cell]
inputCell index move board = take index board ++ [(Occuppied move)] ++ drop (index + 1) board

-- 3 vertical columns
check1 :: Move -> [Cell] -> Bool
check1 move board
   | returnTrue = True
   | otherwise  = False
   where 
       returnTrue = board !! 0 == (Occuppied move) && board !! 3 == (Occuppied move) && board !! 6 == (Occuppied move)
check2 :: Move -> [Cell] -> Bool       
check2 move board
   | returnTrue = True
   | otherwise  = False
   where 
       returnTrue = board !! 1 == (Occuppied move) && board !! 4 == (Occuppied move) && board !! 7 == (Occuppied move)
check3 :: Move -> [Cell] -> Bool       
check3 move board
   | returnTrue = True
   | otherwise  = False
   where 
       returnTrue = board !! 2 == (Occuppied move) && board !! 5 == (Occuppied move) && board !! 8 == (Occuppied move)
-- three horizontal row
check4 :: Move -> [Cell] -> Bool
check4 move board
   | returnTrue = True
   | otherwise  = False
   where 
       returnTrue = board !! 0 == (Occuppied move) && board !! 1 == (Occuppied move) && board !! 2 == (Occuppied move)
check5 :: Move -> [Cell] -> Bool
check5 move board
   | returnTrue = True
   | otherwise  = False
   where 
       returnTrue = board !! 3 == (Occuppied move) && board !! 4 == (Occuppied move) && board !! 5 == (Occuppied move)
check6 :: Move -> [Cell] -> Bool
check6 move board
   | returnTrue = True
   | otherwise  = False
   where 
       returnTrue = board !! 6 == (Occuppied move) && board !! 7 == (Occuppied move) && board !! 8 == (Occuppied move)
-- two diagonal lines
check7 :: Move -> [Cell] -> Bool
check7 move board
   | returnTrue = True
   | otherwise  = False
   where 
       returnTrue = board !! 0 == (Occuppied move) && board !! 4 == (Occuppied move) && board !! 8 == (Occuppied move)
check8 :: Move -> [Cell] -> Bool
check8 move board
   | returnTrue = True
   | otherwise  = False
   where 
       returnTrue = board !! 2 == (Occuppied move) && board !! 4 == (Occuppied move) && board !! 6 == (Occuppied move)

checkWinner :: Move -> [Cell] -> Bool
checkWinner move board
    | returnTrue = True
    | otherwise = False
    where 
        returnTrue = check1 move board|| check2 move board|| check3 move board|| check4 move board|| check5 move board|| check6 move board|| check7 move board|| check8 move board

checkDraw :: [Cell] -> Bool
checkDraw board
    | returnTrue = True
    | otherwise  = False
    where
        returnTrue = board !! 0 /= Empty && board !! 1 /= Empty && board !! 2 /= Empty && board !! 3 /= Empty && board !! 4 /= Empty && board !! 5 /= Empty && board !! 6 /= Empty && board !! 7 /= Empty && board !! 8 /= Empty

getInput :: [Cell] -> IO String
getInput board = do input <- getLine
                    case (checkEmpty (inputConverter input) board) of
                        True -> return input
                        False -> do putStr "INVALID POSITION\n" 
                                    getInput board

run :: [Cell] -> Move -> IO ()
run board move = do printBoard board
                    case (checkDraw board) of
                        True -> do putStr "Draw\n"
                                   return ()
                        False -> do putStr (show move ++ " Move\n")
                                    input <- getInput board
                                    case (checkWinner move (inputCell (inputConverter input) move board)) of 
                                        True -> do printBoard (inputCell (inputConverter input) move board)
                                                   putStr (show move ++ " Win\n")
                                                   return ()
                                        False -> run (inputCell (inputConverter input) move board) (nextMove move)
    

tictactoe :: IO ()
tictactoe = do 
    let board = [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]
    run board O 
