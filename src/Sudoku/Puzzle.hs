{-# LANGUAGE TemplateHaskell #-}
module Sudoku.Puzzle where

import Control.Lens
import Control.Lens.TH
import Data.List.Ordered (nubSort)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import Text.Printf

-- | A coordinate of a puzzle
newtype Cell = Cell { unCell :: (Int, Int) }
  deriving
    ( Show
    , Eq
    , Ord
    )

-- | A list of cells that must have unique contents
type Group = [Cell]

-- | A description of a puzzle layout.
data Puzzle
  = Puzzle
    { _puzzleCells :: [Cell]
    , _puzzleGroups :: [Group]
    , _puzzleAlphabetSize :: Int
    , _puzzleCellGroups :: Cell -> [Group]
    }
makeLenses ''Puzzle

-- | The contents of a Cell
type Value = Int

-- | The contents of a puzzle.
newtype Contents = Contents (M.Map Cell Value)
  deriving
    ( Show
    , Eq
    )

puzzleAlphabet :: Puzzle -> [Int]
puzzleAlphabet puzzle = [1..puzzle ^. puzzleAlphabetSize]

-- | A list of all Groups that contain a Cell.
--
-- This can be used to define the 'puzzleCellGroups' field for a
-- generic puzzle.
defaultCellGroups :: Puzzle -> Cell -> [[Cell]]
defaultCellGroups (Puzzle _ groups _ _) cell
  = filter inGroup groups
  where
    inGroup group = cell `elem` group

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
formatGroup puzzle group = formatContents puzzle contents
  where
    contents = Contents (M.fromList (zip group (puzzleAlphabet puzzle)))

readCell :: Contents -> Cell -> Maybe Value
readCell (Contents contents) cell
  = M.lookup cell contents

fillCell :: Contents -> Cell -> Value -> Contents
fillCell (Contents contents) cell value
  = Contents (M.insert cell value contents)

-- | Get the non-missing entries of a group.
readGroup :: Contents -> Group -> [Value]
readGroup contents group
  = catMaybes (readCell contents <$> group)

emptyContents :: Contents
emptyContents = Contents M.empty
