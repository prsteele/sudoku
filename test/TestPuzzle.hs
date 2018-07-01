module TestPuzzle where

import Control.Monad
import Control.Lens
import qualified Data.Set as S
import Test.Tasty
import Test.Tasty.HUnit
import Text.Printf

import Sudoku.Puzzle
import Sudoku.Puzzle.Standard

test_all :: TestTree
test_all = testGroup "Puzzle manipulation"
  [ standardPuzzle
  ]

standardPuzzle :: TestTree
standardPuzzle = testGroup "9x9"
  [ testCase "Groups in cells" (groupsInPuzzle standard)
  , testCase "Cell groups and groups agree" (groupsAndCellGroupsAgree standard)
  ]
  where
    standard = mkStandardPuzzle

groupsInPuzzle :: Puzzle -> Assertion
groupsInPuzzle puzzle
  = forM_ (puzzle ^. puzzleGroups) assertGroupInPuzzle
  where
    cellSet = S.fromList (puzzle ^. puzzleCells)
    assertGroupInPuzzle group
      = assertEqual message S.empty missing
      where
        missing = S.difference (S.fromList group) cellSet
        message = printf "Group elements %s not in puzzle" (show missing)

groupsAndCellGroupsAgree :: Puzzle -> Assertion
groupsAndCellGroupsAgree (Puzzle cells groups _ cellGroups)
  = groupSet @=? cellGroupsSet
  where
    groupSet = unionAll (S.fromList <$> groups)
    cellGroupsSet = unionAll (S.fromList <$> (concat (cellGroups <$> cells)))

    unionAll = foldr S.union S.empty
