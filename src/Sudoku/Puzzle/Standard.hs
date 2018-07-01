{-# LANGUAGE TupleSections #-}
{-| Standard 9x9 Sudoku puzzles. -}
module Sudoku.Puzzle.Standard where

import Control.Lens
import Control.Monad
import Data.List (sort)
import Data.Maybe (catMaybes)
import qualified Data.Map.Strict as M
import Text.Megaparsec
import Text.Megaparsec.Char

import Sudoku.Puzzle

mkStandardPuzzle :: Puzzle
mkStandardPuzzle
  = Puzzle
  { _puzzleCells = curry Cell <$> alphabet <*> alphabet
  , _puzzleGroups = rows ++ columns ++ blocks
  , _puzzleAlphabetSize = 9
  , _puzzleCellGroups = cellGroups
  }
  where
    alphabet = [0..8]
    blockStarts = filter ((== 0) . (`div` 3)) alphabet

    rows =
      [ [ Cell (row, column)
        | column <- alphabet
        ]
      | row <- alphabet
      ]
    columns =
      [ [ Cell (row, column)
        | row <- alphabet
        ]
      | column <- alphabet
      ]
    blocks =
      [ [ Cell (row + i, column + j)
        | i <- [0..2]
        , j <- [0..2]
        ]
      | row <- blockStarts
      , column <- blockStarts
      ]

    cellGroups :: Cell -> [[Cell]]
    cellGroups (Cell (row, column))
      = cellRow : cellColumn : [cellBlock]
      where
        cellRow = [Cell (row, column') | column' <- alphabet]
        cellColumn = [Cell (row', column) | row' <- alphabet]
        cellBlock =
          [ Cell (row' + i, column' + j)
          | i <- [0..2]
          , j <- [0..2]
          ]
          where
            row' = (row `div` 3) * 3
            column' = (column `div` 3) * 3

type Parser = Parsec () String

entry :: Parser (Maybe Value)
entry = Just . read . return <$> (oneOf "123456789" <?> "entry")

missing :: Parser (Maybe Value)
missing = const Nothing <$> oneOf " .0"

cell :: Parser (Maybe Value)
cell = do
  value <- entry <|> missing
  many eol
  return value

contents :: Parser Contents
contents = build <$> replicateM 81 cell
  where
    cells = sort (mkStandardPuzzle ^. puzzleCells)
    build entries = Contents . M.fromList . catMaybes $ mEntries
      where
        mEntries :: [Maybe (Cell, Value)]
        mEntries = zipWith (fmap . (,)) cells entries
