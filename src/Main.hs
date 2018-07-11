module Main where

import System.Environment
import Text.Parsec

import Parser
import Data
import Generate

main :: IO ()
main = do
       filename <- (!! 0) <$> getArgs
       code <- readFile filename
       writeFile (svgFilename filename) (genPuzzle $
                                         either (\_ -> error "Parse error") id $ parse puzma "" code)
       where svgFilename = (++ ".svg") . takeWhile (\c -> c /= '.')