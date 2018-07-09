module PuzmaParser where

import Text.Parsec
import Data.Char

import PuzmaData

type Parser a = Parsec String () a

puzma :: Parser Puzzle
puzma = do
        spaces >> string "RectangleGrid" >> spaces >> char '{' 
        gridProperties <- sepBy gridProperty (char ',')
        spaces >> char '}' >> spaces
        return Puzzle { grid = Rectangle { height = maybe (error "Error: grid has undefined height") id $ lookup "height" gridProperties,
                                           width = maybe (error "Error: grid has undefined width") id $ lookup "width" gridProperties },
                        objects = [] }

gridProperty :: Parser (String, Int)
gridProperty = do
               spaces
               key <- many1 letter
               spaces >> char ':' >> spaces
               value <- read <$> many1 digit
               spaces
               return (key, value)
               
notSpace :: Parser Char
notSpace = satisfy (not . isSpace)