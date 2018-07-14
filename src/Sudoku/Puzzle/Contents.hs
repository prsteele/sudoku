module Sudoku.Puzzle.Contents where

import Control.Lens
import Data.List.Ordered (nubSort)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)

import Text.Printf

import Sudoku.Puzzle.Puzzle

-- | The contents of a Cell
type Value = Int

-- | The contents of a puzzle.
newtype Contents = Contents { unContents :: (M.Map Cell Value) }
  deriving
    ( Show
    , Eq
    )

readCell :: Contents -> Cell -> Maybe Value
readCell (Contents contents) cell
  = M.lookup cell contents

fillCell :: Contents -> Cell -> Value -> Contents
fillCell (Contents contents) cell value
  = Contents (M.insert cell value contents)

-- | Get the non-missing entries of a group.
readGroup :: Contents -> Group -> [Value]
readGroup contents (Group group)
  = catMaybes (readCell contents <$> group)

emptyContents :: Contents
emptyContents = Contents M.empty

formatContents :: Puzzle -> Contents -> String
formatContents puzzle contents
  = unlines
  [ concat
    [ formatValue (readCell contents (Cell (row, column)))
    | column <- columns]
  | row <- rows
  ]
  where
    cellRow :: Cell -> Int
    cellRow = fst . unCell

    cellColumn :: Cell -> Int
    cellColumn = snd . unCell

    rows = nubSort (cellRow <$> puzzle ^. puzzleCells)
    columns = nubSort (cellColumn <$> puzzle ^. puzzleCells)

    formatValue :: Maybe Value -> String
    formatValue Nothing  = " . "
    formatValue (Just x) = printf " %i " x

formatGroup :: Puzzle -> Group -> String
formatGroup puzzle (Group group) = formatContents puzzle contents
  where
    contents = Contents (M.fromList (zip group (puzzleAlphabet puzzle)))
