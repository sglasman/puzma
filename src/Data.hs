module Data where

data Puzzle = Puzzle { mGrid :: Grid,
                       mObjects :: [Object] } deriving (Show)
    
data Grid = Rectangle { height :: Int,
                        width :: Int,
                        gridsize :: Int } deriving (Show)
                        
data Object = Object { content :: String,
                     location :: GridCoord }deriving (Show)


type GridCoord = (Int, Int)
type PixelCoord = (Int, Int)