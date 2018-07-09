module PuzmaData where

data Puzzle = Puzzle { grid :: Grid,
                       objects :: [Object] } deriving (Show)
    
data Grid = Rectangle { height :: Int,
                        width :: Int } deriving (Show)
                        
data Object = Clue String deriving (Show)