module Main where

import Text.Megaparsec

import Sudoku.Puzzle
import Sudoku.Puzzle.Standard
import Sudoku.Search

main :: IO ()
main = do
  contentsData <- getContents
  case parse parseContents "stdin" contentsData of
    Left e -> putStrLn (parseErrorPretty e)
    Right contents -> do
      putStrLn "Solving:"
      putStrLn (formatContents mkStandardPuzzle contents)
      putStrLn "Result:"
      case solve contents of
        Nothing     -> putStrLn "No solution"
        Just result -> putStrLn (formatContents mkStandardPuzzle result)

solve :: Contents -> Maybe Contents
solve contents = case solvePuzzle mkStandardPuzzle contents of
  (x: _) -> Just x
  _      -> Nothing
