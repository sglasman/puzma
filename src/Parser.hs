module Parser where

import Text.Parsec
import Data.Char

import Data

type Parser a = Parsec String () a

puzma :: Parser Puzzle
puzma = do
        puzzleGrid <- gridP
        gridObjects <- many objectP
        return Puzzle { mGrid = puzzleGrid,
                        mObjects = gridObjects }

gridP :: Parser Grid
gridP = do
        spaces >> string "RectangleGrid" >> spaces >> char '{'
        gridProperties <- sepBy propertyP (spaces >> char ',')
        char '}'
        return Rectangle { height = maybe (error "Error: grid has undefined height") id $ lookup "height" gridProperties,
                           width = maybe (error "Error: grid has undefined width") id $ lookup "width" gridProperties,
                           gridsize = maybe 36 id $ lookup "gridsize" gridProperties }

objectP :: Parser Object
objectP = do
          spaces >> string "at"
          clueLocation <- coordinateP
          spaces >> string "Clue" >> spaces >> char '{'
          clueContent <- many $ satisfy (/= '}')
          return Clue { content = clueContent, location = clueLocation }

propertyP :: Parser (String, Int)
propertyP = do
           spaces
           key <- many1 letter
           spaces >> char ':' >> spaces
           value <- read <$> many1 digit
           return (key, value)

coordinateP :: Parser GridCoord
coordinateP = do
              spaces >> char '(' >> spaces
              xValue <- read <$> many1 digit
              spaces >> char ',' >> spaces
              yValue <- read <$> many1 digit
              spaces >> char ')'
              return (xValue, yValue)
               
notSpace :: Parser Char
notSpace = satisfy (not . isSpace)