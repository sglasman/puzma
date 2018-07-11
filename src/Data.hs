module Data where

data Puzzle = Puzzle { grid :: Grid,
                       objects :: [Object] } deriving (Show)
    
data Grid = Rectangle { height :: Int,
                        width :: Int,
                        gridsize :: Int } deriving (Show)
                        
data Object = Clue { content :: String,
                     location :: Coord }deriving (Show)

type Coord = (Int, Int)