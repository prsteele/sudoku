module TestRules where

import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Map.Strict as M
import Test.Tasty
import Test.Tasty.HUnit

import Sudoku.Puzzle
import Sudoku.Rules

test_rules :: TestTree
test_rules = testGroup "Rules"
  [ smallTests
  ]

smallTests :: TestTree
smallTests = testGroup "2x2"
  [ testCase "Duplicate row entry" assert2x2RowContradiction
  , testCase "Duplicate column entry" assert2x2ColumnContradiction
  , testCase "Incomplete solution" assert2x2Incomplete
  , testCase "Complete solution" assert2x2Complete
  ]

-- | A 2x2 puzzle, with groups of rows and columns
smallPuzzle :: Puzzle
smallPuzzle
  = Puzzle
  { _puzzleCells = curry Cell <$> [0, 1] <*> [0, 1]
  , _puzzleGroups = [ [Cell (0, 0), Cell (1, 0)]
                    , [Cell (0, 1), Cell (1, 1)]
                    , [Cell (0, 0), Cell (0, 1)]
                    , [Cell (1, 0), Cell (1, 1)]
                    ]
  , _puzzleAlphabetSize = 2
  , _puzzleCellGroups = defaultCellGroups smallPuzzle
  }

assert2x2RowContradiction :: Assertion
assert2x2RowContradiction
  = assertEqual "Expected a contradiction" witness status
  where
    group = [Cell (0, 0), Cell (0, 1)]
    status = puzzleStatus smallPuzzle badContents
    witness = Witness ((Contradiction group 1) :| [])

    badContents :: Contents
    badContents = Contents (M.fromList [(Cell (0, 0), 1), (Cell (0, 1), 1)])

assert2x2ColumnContradiction :: Assertion
assert2x2ColumnContradiction
  = assertEqual "Expected a contradiction" witness status
  where
    group = [Cell (0, 0), Cell (1, 0)]
    status = puzzleStatus smallPuzzle badContents
    witness = Witness ((Contradiction group 1) :| [])

    badContents :: Contents
    badContents = Contents (M.fromList [(Cell (0, 0), 1), (Cell (1, 0), 1)])

assert2x2Incomplete :: Assertion
assert2x2Incomplete
  = assertEqual "Expected to be incomplete" Incomplete status
  where
    status = puzzleStatus smallPuzzle contents

    contents :: Contents
    contents = Contents (M.fromList [(Cell (0, 0), 1), (Cell (1, 0), 2)])

assert2x2Complete :: Assertion
assert2x2Complete
  = assertEqual "Expected to be complete" Complete status
  where
    status = puzzleStatus smallPuzzle contents

    cells
      = [ (Cell (0, 0), 1)
        , (Cell (0, 1), 2)
        , (Cell (1, 0), 2)
        , (Cell (1, 1), 1)
        ]

    contents :: Contents
    contents = Contents (M.fromList cells)
