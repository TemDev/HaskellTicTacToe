module TicTacToe where

import Data.Char
import Data.Maybe
import Data.List
import Text.Read

-------------------------------------------------------------------
data Player = O | X
            deriving (Eq, Show)

data Cell = Empty | Taken Player
          deriving (Eq, Show)

type Board = ([Cell], Int)

type Position = (Int, Int)

-------------------------------------------------------------------

--
-- Some useful functions from, or based on, the unassessed problem sheets...
--

-- Preserves Just x iff x satisfies the given predicate. In all other cases
-- (including Nothing) it returns Nothing.
filterMaybe :: (a -> Bool) -> Maybe a -> Maybe a
filterMaybe p m@(Just x)
  | p x = m
filterMaybe p _
  = Nothing

-- Replace nth element of a list with a given item.
replace :: Int -> a -> [a] -> [a]
replace 0 p (c : cs)
  = p : cs
replace _ p []
  = []
replace n p (c : cs)
  = c : replace (n - 1) p cs

-- Returns the rows of a given board.
rows :: Board -> [[Cell]]
rows (cs , n)
  = rows' cs
  where
    rows' []
      = []
    rows' cs
      = r : rows' rs
      where
        (r, rs) = splitAt n cs

-- Returns the columns of a given board.
cols :: Board -> [[Cell]]
cols
  = transpose . rows

-- Returns the diagonals of a given board.
diags :: Board -> [[Cell]]
diags (cs, n)
  = map (map (cs !!)) [[k * (n + 1) | k <- [0 .. n - 1]],
                      [k * (n - 1) | k <- [1 .. n]]]

-------------------------------------------------------------------

gameOver :: Board -> Bool
gameOver board = check (rows board) || check (cols board) || check (diags board) || notElem Empty (fst board)
  where
    check :: [[Cell]] -> Bool
    check [] = False
    check (r:rs) = (notElem Empty r && ((Taken X `elem` r) /= (Taken O `elem` r))) || check rs

-------------------------------------------------------------------

--
-- Moves must be of the form "row col" where row and col are integers
-- separated by whitespace. Bounds checking happens in tryMove, not here.
--
parsePosition :: String -> Maybe Position
parsePosition "" = Nothing
parsePosition xxs
  | isNothing(readMaybe x :: Maybe Int) || isNothing(readMaybe y :: Maybe Int) = Nothing
  | otherwise = Just (read x, read y)
    where
      locatespace :: String -> Int -> Int
      locatespace (x:xs) count
        | x == ' ' = count
        | otherwise = locatespace xs (count + 1)
      spaceindex = locatespace xxs 1
      x = take (spaceindex - 1) xxs
      y = drop spaceindex xxs

tryMove :: Player -> Position -> Board -> Maybe Board
tryMove p (x, y) (board, size)
  | x < 0 || y < 0 || x >= size || y >= size = Nothing 
  | otherwise = if Taken O == val || Taken X == val then Nothing else Just (take pos board ++ [Taken p] ++ drop (pos + 1) board, size)
    where
      pos = size * x + y
      val = board !! pos
-------------------------------------------------------------------
-- I/O Functions

{-
prettyPrint :: Board -> IO ()
prettyPrint ([], size) = putStr ""
prettyPrint (x:xs, size) = do 
                              if length xs `mod` size == 0 then putStrLn addend else putStr addend
                              prettyPrint (xs, size)
                                where 
                                  mapping = [(Taken O, "O "), (Taken X, "X "), (Empty, "- ")]
                                  addend = fromJust $ lookup x mapping
-}
prettyPrint :: Board -> IO ()
prettyPrint board = putStr (concat (map (\x -> (intersperse ' ' (concat $ map (\y -> fromJust $ lookup y mapping) x)) ++ "\n") (rows board)))
  where
    mapping = [(Taken O, "O"), (Taken X, "X"), (Empty, "-")]
-- The following reflect the suggested structure, but you can manage the game
-- in any way you see fit.

-- Repeatedly read a target board position and invoke tryMove until
-- the move is successful (Just ...).
takeTurn :: Board -> Player -> IO Board
takeTurn board p = doParseAction (\x -> if isNothing (parsePosition x) then Nothing else tryMove p (fromJust (parsePosition x)) board)

doParseAction :: (String -> Maybe a) -> IO a
doParseAction f = do
                    inp <- getLine
                    if isNothing (f inp) then
                      do 
                        putStr "Invalid input, try again :O : "
                        doParseAction f
                    else 
                      return (fromJust (f inp))
-- Manage a game by repeatedly: 1. printing the current board, 2. using
-- takeTurn to return a modified board, 3. checking if the game is over,
-- printing the board and a suitable congratulatory message to the winner
-- if so.
playGame :: Board -> Player -> IO ()
playGame board p = do
                     prettyPrint board
                     putStr ("Player " ++ show p ++  " make your move: ")
                     t <- takeTurn board p
                     if gameOver t then 
                       putStrLn ("Player " ++ show p ++ " has won the game")
                     else 
                       playGame t (fromJust (lookup p mapping))
                         where
                          mapping = [(O, X), (X, O)]


                   

-- Print a welcome message, read the board dimension, invoke playGame and
-- exit with a suitable message.
main :: IO ()
main = do
         putStrLn "Welcome to Tic Tac Toe"
         putStr "Choose a size of the board: "
         size <- getLine
         playGame (take (read size * read size) $ repeat Empty, read size) X
         putStrLn "Thank you for playing!"

-------------------------------------------------------------------

testBoard1, testBoard2, testBoard3 :: Board

testBoard1
  = ([Taken O,Taken X,Empty,Taken O,
      Taken O,Empty,Taken X,Taken X,
      Taken O,Empty,Empty,Taken X,
      Taken O,Taken X,Empty,Empty],
      4)

testBoard2
  = ([Taken X,Empty,
      Empty,Empty],
      2)

testBoard3
  = ([Taken O,Taken X,Empty,Taken O,Taken X,
      Taken O,Empty,Taken X,Taken X,Empty,
      Empty,Empty,Taken X,Taken O,Taken O,
      Taken O,Taken X,Empty,Empty,Taken X,
      Taken X,Empty,Taken O,Empty,Empty],
      5)
