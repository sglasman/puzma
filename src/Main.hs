module Main where

import System.Environment
import Text.Parsec

import Parser
import Data
import Generate

main :: IO ()
main = do
       args <- getArgs
       let l = length args
       case l of
              0 -> error "Input filename required"
              1 -> putStrLn "Output filename not supplied: will use default derived from input filename"
              _ -> return ()
       inputFilename <- (!! 0) <$> getArgs
       let outputFilename = if l == 1 then defaultFilename inputFilename else args !! 1
       code <- readFile inputFilename
       either (putStrLn . show)
              (writeFile outputFilename . genPuzzle)
              (parse puzma "" code)
       where defaultFilename = reverse . ("gvs" ++) . dropWhile (/= '.') . reverse