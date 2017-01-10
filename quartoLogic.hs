{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}

------------------ Quarto ------------------
--
-- Author: Kyle Hersey
-- Course: CIS*4900 - functional programming
--
-- Please excuse me for putting all my code in one file
-- It's not pretty but it works!
--
-- Tested on The Glorious Glasgow Haskell Compilation System, version 8.0.1
--
--------------------------------------------

-- TO RUN:
-- In terminal run "ghci"
-- in ghci run command ":load quartoLogic.hs"
-- once loaded, enter "main" and the game will start

-- NOTE: if you make a mistake while playing the game and press backspace it will break everything

-- References:
-- http://stackoverflow.com/questions/5852722/replace-individual-list-elements-in-haskell
-- http://keera.co.uk/blog/2013/03/19/creating-board-games-in-haskell/
-- http://stackoverflow.com/questions/21885530/how-to-input-several-values-in-one-line-in-haskell

import System.IO
import Data.Char


-- DATA & TYPE DECLARATIONS START --

class GameRules a index tile player piece | a -> index, a -> tile, a -> player, a -> piece where

  localPlayer     :: a -> player
  curPlayer       :: a -> player
  getPieces       :: a -> [piece]
  getBoard        :: a -> [[tile]]
  toPlace         :: a -> tile -- tile can be Empty or Piece

  -- Is it the current player's move (only applies to online play)
  moveEnabled     :: a -> Bool
  moveEnabled _ = False

  -- Does the player have a piece to player? (should only return false on first turn, or stalemate)
  hasPlacePiece   :: a -> Bool
  hasPlacePiece _ = False

  -- Is a position on the board empty
  canPlacePiece   :: a -> (index,index) -> Bool
  canPlacePiece _ _ = False

  -- Has the game been won
  hasWinState     :: a -> Bool
  hasWinState _ = False

  -- Place piece, update board state and toPlace value
  placePiece      :: a -> (index,index) -> a
  placePiece g _ = g

  -- Get unplaced piece by index
  getPiece        :: a -> index -> piece

  -- Update list of unplaced pieces, removing one piece and adding it to toPlay
  setPlacePiece   :: a -> piece -> a
  setPlacePiece g _ = g

  -- Set local player (for online games)
  setLocalPlayer  :: a -> player -> a
  setLocalPlayer g _  = g

  -- Returns a string, either "Player 1" or "Player 2" for CLI purposes
  getCurPlayerStr :: a -> String

  -- These are for local games
  toggleCurPlayer    :: a -> a
  toggleLocalPlayer  :: a -> a
-- GameRules ends here --

-- 16 unique types of Piece
data Piece = Piece { 
  height :: Char, 
  holes :: Char,
  colour :: Char,
  shape :: Char
  } 
  deriving(Show, Eq)

-- 2 types of Tile
data Tile a = Full a | Empty deriving (Eq)

-- 2 types of Player
data Player = P1 | P2 deriving (Eq, Show)

-- GameState we will be attaching to the ruleset
data GameState tile player piece = GameState {
  localPlayer'   :: player,
  curPlayer'     :: player,
  boardState     :: [[tile]],
  unplacedPieces :: [piece],
  placementPiece :: tile
}

newtype QuartoGame = QuartoGame (GameState (Tile Piece) Player Piece)
-- DATA DECLARATIONS END --


-- WIN STATE CALCULATION STARTS --
-- Takes in the entire board and returns a list containing all the possible winning vectors
makeEvalRows :: [[Tile Piece]] -> [[Tile Piece]]
makeEvalRows [] = []
makeEvalRows board = diag1 : diag2 : row1 : row2 : row3 :row4 : col1 : col2 : col3 : col4 :[]
  where 
    diag1 = board!!0!!0 : board!!1!!1 : board!!2!!2 : board!!3!!3 :[]
    diag2 = board!!0!!3 : board!!1!!2 : board!!2!!1 : board!!3!!0 :[]
    col1 = board!!0
    col2 = board!!1
    col3 = board!!2
    col4 = board!!3
    row1 = board!!0!!0 : board!!1!!0 : board!!2!!0 : board!!3!!0 :[]
    row2 = board!!0!!1 : board!!1!!1 : board!!2!!1 : board!!3!!1 :[]
    row3 = board!!0!!2 : board!!1!!2 : board!!2!!2 : board!!3!!2 :[]
    row4 = board!!0!!3 : board!!1!!3 : board!!2!!3 : board!!3!!3 :[]

-- Removes Pieces from Tiles. To be used with Map.
-- Note this does not account for Empty Tiles, so must be used like this:
--    map extractPieces $ filter noEmptyTiles $ [someListOfTiles]
extractPieces :: [Tile Piece] -> [Piece]
extractPieces [] = []
extractPieces ((Full x):xs) = x : extractPieces xs

-- Checks if a row of Pieces shares a common attribute
-- The passed function exstracts a single attribute from each Piece
hasWin :: (Piece -> Char) -> [Piece] -> Bool
hasWin _ []   = False
hasWin f (x1:x2:x3:x4:xs)
  | (f x1) == (f x2) && (f x1) == (f x3) && (f x1) == (f x4) = True
  | otherwise = False

-- Evaluates whether a list of pieces contains a win-state
evaluateRows :: [[Piece]] -> Bool
evaluateRows []     = False
evaluateRows (x:xs) = hasWin height x || hasWin shape x || hasWin colour x || hasWin holes x || evaluateRows xs

-- Returns true if a row contains no empty Tiles, to be used with Filter
noEmptyTiles :: [Tile Piece] -> Bool
noEmptyTiles [] = True
noEmptyTiles (x:xs) 
  | x == Empty  = False
  | otherwise   = noEmptyTiles xs
-- WIN STATE CALCULATION ENDS --


-- CLI OUTPUT STARTS -- 
-- Prints a list of all pieces, enumerated
printAllPieces :: QuartoGame -> IO ()
printAllPieces game = mapM_ putStrLn $ enumPiecesToStr $ enumerate $ getPieces game

-- Prints the entire board looking like this:
--   x  0     1     2     3  
-- y +-----------------------+
--   | [ ] | (o) | [ ] | (o) |
-- 0 |  B  | (W) | [W] |  B  |
--   +-----------------------+
--   | [o] |     |     |     |
-- 1 |  B  |     |     |     |
--   +-----------------------+
--   | [o] |     |     |     |
-- 2 | [B] |     |     |     |
--   +-----------------------+
--   | [ ] |     |     |     |
-- 3 |  W  |     |     |     |
--   +-----------------------+
printBoard :: QuartoGame -> IO ()
printBoard game = mapM_ putStrLn $ buildBoardStr $ getBoard game

-- Returns a copy of a List with each element enumerated
enumerate :: [a] -> [(Int,a)]
enumerate [] = []
enumerate x = zip [0..] x

-- Converts a Tile into a String tuple for printing
tileToStr :: (Tile Piece) -> (String,String)
tileToStr Empty = ("     ","     ")
tileToStr (Full p) = pieceToStr p

-- Turns a Piece String tuple into a single String
flattenTileTuple :: (String,String) -> String
flattenTileTuple (x,y) = x ++ "\n" ++ y

-- Creates a string representation of the board
buildBoardStr :: [[(Tile Piece)]] -> [String]
buildBoardStr board = first : second : row0 : fill : row1 : fill : row2 : fill : row3 : fill : []
  where 
    first  = "   x  0     1     2     3   "
    second = " y +-----------------------+"
    fill   = "   +-----------------------+"
    row0   = buildRowStr (board!!0) 0
    row1   = buildRowStr (board!!1) 1
    row2   = buildRowStr (board!!2) 2
    row3   = buildRowStr (board!!3) 3

-- Creates a single row on the board, which is 2 characters high
buildRowStr :: [(Tile Piece)] -> Int -> String
buildRowStr row i = "   |" ++ top0 ++ "|" ++ top1 ++ "|" ++ top2 ++ "|" ++ top3 ++ "|\n " ++ (show i) ++ " |" ++ bot0 ++ "|" ++ bot1 ++ "|" ++ bot2 ++ "|" ++ bot3 ++ "|" 
  where
    (top0,bot0) = tileToStr (row!!0)
    (top1,bot1) = tileToStr (row!!1)
    (top2,bot2) = tileToStr (row!!2)
    (top3,bot3) = tileToStr (row!!3)

-- Creates String representations for each enumerated Piece
enumPiecesToStr :: [(Int,Piece)] -> [String]
enumPiecesToStr [] = []
enumPiecesToStr ((i,x):xs) = (enumerator ++ first ++ "\n" ++ last ++ "\n" ) : (enumPiecesToStr xs)
  where
    (first, last) = (pieceToStr x)
    enumerator = if (length $ show i) == 2
                  then ((show i) ++ ".  \n")
                else ((show i) ++ ".   \n")

-- Converts a Piece into it's string representation, which is 2 Strings high
pieceToStr :: Piece -> (String,String)
pieceToStr p = (top,bottom)
  where
    top = if (holes p) == 'h'
          then if (shape p) == 'b'
                then " [o] "
              else " (o) "
          else if (shape p) == 'b'
                then " [ ] "
              else " ( ) "
    bottom = if (height p) == 't'
            then if (shape p) == 'b'
                then if (colour p) == 'd'
                      then " [B] "
                    else " [W] "
                else if (colour p) == 'd'
                      then " (B) "
                    else " (W) "
            else if (colour p) == 'd'
                  then "  B  "
                else "  W  "
-- CLI OUTPUT ENDS --



-- CLI INPUT STARTS --
-- Facilitates input for selecting the next piece to place
selectPieceIndex :: QuartoGame -> IO Int
selectPieceIndex game = do
  printAllPieces game
  validatePieceInt game

-- Ensures the user inputs 1 Int that refers to a Piece that has not been placed yet
validatePieceInt :: QuartoGame -> IO Int
validatePieceInt game = do
  putStrLn $ (getCurPlayerStr game) ++ " Selecting Piece for other player"
  putStrLn "Please enter the index of the piece"
  i <- read1Int
  if i >= 0 && i < length (getPieces game)
    then return i
  else do
    putStrLn "sorry, that is not a valid index!"
    validatePieceInt game

-- Faciliates user input for placing a piece on the board
selectPlacePos :: QuartoGame -> IO [Int]
selectPlacePos game = do
  printBoard game
  validateInts game

-- Ensures the user enters 2 valid integers refering to a location on the board
validateInts :: QuartoGame -> IO [Int]
validateInts game = do
  putStrLn $ (getCurPlayerStr game) ++ " Placing Piece :"
  putStrLn $ flattenTileTuple $ tileToStr $ toPlace game
  putStrLn "Please Enter X and Y coordinates seperated by a space"
  ints <- read2Ints
  let iX = ints!!0
  let iY = ints!!1
  if validatePosition (iX,iY) && canPlacePiece game (iX,iY)
    then return ints
  else do
    putStrLn "sorry, that position is either invalid, or already filled"
    validateInts game

-- Returns True if a set of coordinates are on the board
validatePosition :: (Int,Int) -> Bool
validatePosition (x,y)
  | x > 3 || y > 3 = False
  | x < 0 || y < 0 = False
  | otherwise = True

-- Reads in any numbers of Ints from the command line
readInts :: IO [Int]
readInts = fmap (map read.words) getLine

-- Ensures 2 Ints are entered via command line
read2Ints :: IO [Int]
read2Ints = do
     putChar '>'
     ints <- readInts
     if length ints == 2 then return ints  -- return ints
      else do
         putStrLn ("sorry, could you give me exactly 2 integers, "
                  ++ "separated by spaces?")
         read2Ints

-- Ensures a single Int is entered via command line
read1Int :: IO Int
read1Int = do 
  putChar '>'
  ints <- readInts
  if length ints == 1 then return $ ints!!0
    else do
      putStrLn ("sorry, could you give me exactly 1 integer")
      read1Int

-- CLI INPUT ENDS --


-- Default game state --
defaultQuartoGame :: QuartoGame
defaultQuartoGame = QuartoGame $ GameState {
  localPlayer'    = P1,
  curPlayer'      = P1,
  boardState     = emptyBoard,
  unplacedPieces = allThePieces,
  placementPiece = Empty
} where emptyBoard   = [[Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty]]
        allThePieces = [(Piece height holes colour shape) | height <- ['t','s'], holes <- ['h','u'], colour <- ['d','l'], shape <- ['b','c']]

-- GameRules Implementation --
-- You can find Documentation in the GameRules class definition.
instance GameRules QuartoGame Int (Tile Piece) Player Piece where
  localPlayer (QuartoGame game) = localPlayer' game
  curPlayer   (QuartoGame game) = curPlayer' game
  getBoard    (QuartoGame game) = boardState game
  getPieces   (QuartoGame game) = unplacedPieces game
  toPlace     (QuartoGame game) = placementPiece game

  moveEnabled (QuartoGame game) 
    | (localPlayer' game) == (curPlayer' game) = True
    | otherwise = False

  hasPlacePiece (QuartoGame game)
    | (placementPiece game) == Empty = False
    | otherwise = True

  canPlacePiece (QuartoGame game) (x,y)
    | not (x `elem` [0..3]) || not (y `elem` [0..3]) = False
    | not $ tileOccupied $ getTile (y,x) $ boardState game = True
    | otherwise = False 

  placePiece (QuartoGame game) (x,y) = QuartoGame $ GameState {
      localPlayer' = (localPlayer' game),
      curPlayer'   = (curPlayer' game),
      boardState   = newBoardState,
      unplacedPieces = (unplacedPieces game),
      placementPiece = Empty
    } 
    where
      newBoardState = safeReplaceList (boardState game) (y,x) (placementPiece game)

  setPlacePiece (QuartoGame game) p = QuartoGame $ GameState {
      localPlayer' = (localPlayer' game),
      curPlayer'   = (curPlayer' game),
      boardState   = (boardState game),
      unplacedPieces = newPieceState,
      placementPiece = (Full p)
    }
    where
      newPieceState = filter (/= p) $ (unplacedPieces game)

  setLocalPlayer (QuartoGame game) p = QuartoGame $ GameState {
      localPlayer' = p,
      curPlayer'   = (curPlayer' game),
      boardState   = (boardState game),
      unplacedPieces = (unplacedPieces game),
      placementPiece = (placementPiece game)
    }

  toggleLocalPlayer (QuartoGame game) = QuartoGame $ GameState {
      localPlayer' = newPlayer,
      curPlayer'   = (curPlayer' game),
      boardState   = (boardState game),
      unplacedPieces = (unplacedPieces game),
      placementPiece = (placementPiece game)
    }
    where 
      newPlayer = case localPlayer' game of
        P1 -> P2
        P2 -> P1

  toggleCurPlayer (QuartoGame game) = QuartoGame $ GameState {
      localPlayer' = (localPlayer' game),
      curPlayer'   = newPlayer,
      boardState   = (boardState game),
      unplacedPieces = (unplacedPieces game),
      placementPiece = (placementPiece game)
    }
    where 
      newPlayer = case curPlayer' game of
        P1 -> P2
        P2 -> P1

  getPiece (QuartoGame game) i = (unplacedPieces game)!!i

  getCurPlayerStr (QuartoGame game) = case curPlayer' game of 
    P1 -> "Player 1"
    P2 -> "Player 2"

  hasWinState (QuartoGame game) = evaluateRows $ map extractPieces $ filter noEmptyTiles $ makeEvalRows (boardState game)

-- GAMERULES IMPLEMENTATION HELPER FUNCTIONS START --
-- Returns true if a Tile is Empty
tileOccupied :: Tile a -> Bool
tileOccupied Empty = False
tileOccupied (Full _)= True

-- Returns the tile at a given index
getTile :: (Int,Int) -> [[Tile a]] -> Tile a
getTile (x,y) b = b!!x!!y -- might be -> b!!y!!x

-- Facilitates replacement of an element in a list at a given index
safeReplaceElem :: [a] -> Int -> a -> [a]
safeReplaceElem xs i new =
  if i >= 0 && i < length xs
    then replaceElement xs i new
  else xs

-- Replaces element at a position with another passed in element using recursion
replaceElement :: [a] -> Int -> a -> [a]
replaceElement xs i new = fore ++ (new : aft)
  where fore = take i xs
        aft = drop (i+1) xs

-- Facilitates replacement of a single element in a 2D list
safeReplaceList :: [[a]] -> (Int,Int) -> a -> [[a]]
safeReplaceList xs (i,y) new =
  if i >= 0 && i < length xs
    then replaceList xs (i,y) new
  else xs

-- Implements replacement of a single element in a 2D list using recursion
replaceList :: [[a]] -> (Int,Int) -> a -> [[a]]
replaceList xs (i,y) new = fore ++ ((safeReplaceElem target y new) : aft)
  where fore = take i xs
        target = xs!!i
        aft = drop (i+1) xs

-- GAMERULES IMPLENTATION HELP FUNCTIONS END --
 

-- GAME LOOP & MAIN LOGIC -- 
-- This game loop executes a local 2 player game of Quarto
localGameLoop :: QuartoGame -> IO ()
localGameLoop game = do
  putStrLn $ "It is " ++ (getCurPlayerStr game) ++ "'s turn!"

  if hasPlacePiece game
    then do 
      -- Entire turn -> place piece and select piece
      ints <- selectPlacePos game
      let iX = ints!!0
      let iY = ints!!1
      let game2 = placePiece game (iX,iY)
      printBoard game2
      if hasWinState game2
        then putStrLn $ "***************************\nGAME OVER: " ++ (getCurPlayerStr game) ++ " Has Won!!!\n***************************"
      else do
        pI <- selectPieceIndex game2
        let piece = getPiece game2 pI
        let game3 = setPlacePiece game2 piece
        localGameLoop $ toggleCurPlayer $ toggleLocalPlayer game3 -- RESTART LOOP
  else do
    -- This block should only happen for the first player on the first turn
    pI <- selectPieceIndex game
    let piece = getPiece game pI
    let game1 = setPlacePiece game piece
    localGameLoop $ toggleCurPlayer $ toggleLocalPlayer game1 -- RESTART LOOP

-- Look at the itty-bitty main function, d'awwwww
main :: IO ()
main = do
  localGameLoop defaultQuartoGame

-- FIN --
