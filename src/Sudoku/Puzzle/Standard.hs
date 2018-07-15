{-# LANGUAGE TupleSections #-}
{-| Standard 9x9 Sudoku puzzles. -}
module Sudoku.Puzzle.Standard where

import Control.Lens
import Control.Monad
import Data.List (sort)
import Data.Maybe (catMaybes)
import Data.Void
import qualified Data.Map.Strict as M
import Text.Megaparsec
import Text.Megaparsec.Char

import Sudoku.Puzzle.Puzzle
import Sudoku.Puzzle.Contents

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
      [ Group
        [ Cell (row, column)
        | column <- alphabet
        ]
      | row <- alphabet
      ]
    columns =
      [ Group
        [ Cell (row, column)
        | row <- alphabet
        ]
      | column <- alphabet
      ]
    blocks =
      [ Group
        [ Cell (row * 3 + i, column * 3 + j)
        | i <- [0..2]
        , j <- [0..2]
        ]
      | row <- blockStarts
      , column <- blockStarts
      ]

    cellGroups :: Cell -> [Group]
    cellGroups (Cell (row, column))
      = cellRow : cellColumn : [cellBlock]
      where
        cellRow = Group [Cell (row, column') | column' <- alphabet]
        cellColumn = Group [Cell (row', column) | row' <- alphabet]
        cellBlock =
          Group
          [ Cell (row' + i, column' + j)
          | i <- [0..2]
          , j <- [0..2]
          ]
          where
            row' = (row `div` 3) * 3
            column' = (column `div` 3) * 3

type Parser = Parsec Void String

parseEntry :: Parser (Maybe Value)
parseEntry = Just . read . return <$> (oneOf "123456789" <?> "entry")

parseMissing :: Parser (Maybe Value)
parseMissing = const Nothing <$> oneOf " .0"

parseCell :: Parser (Maybe Value)
parseCell = do
  value <- parseEntry <|> parseMissing
  many eol
  return value

parseContents :: Parser Contents
parseContents = build <$> replicateM 81 parseCell
  where
    cells = sort (mkStandardPuzzle ^. puzzleCells)
    build entries = Contents . M.fromList . catMaybes $ mEntries
      where
        mEntries :: [Maybe (Cell, Value)]
        mEntries = zipWith (fmap . (,)) cells entries
