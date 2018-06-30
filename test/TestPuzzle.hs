module TestPuzzle where

import Control.Monad
import qualified Data.Set as S
import Text.Printf

import Test.Tasty
import Test.Tasty.HUnit

import Sudoku.Puzzle

test_standardPuzzle :: TestTree
test_standardPuzzle = testGroup "9x9"
  [ testCase "Groups in cells" (groupsInPuzzle standard)
  , testCase "Cell groups and groups agree" (groupsAndCellGroupsAgree standard)
  ]
  where
    standard = mkStandardPuzzle

groupsInPuzzle :: (Show a, Ord a) => Puzzle a -> Assertion
groupsInPuzzle (Puzzle cells groups _ _)
  = forM_ groups assertGroupInPuzzle
  where
    cellSet = S.fromList cells
    assertGroupInPuzzle group
      = assertEqual message S.empty missing
      where
        missing = S.difference (S.fromList group) cellSet
        message = printf "Group elements %s not in puzzle" (show missing)

groupsAndCellGroupsAgree :: (Show a, Ord a) => Puzzle a -> Assertion
groupsAndCellGroupsAgree (Puzzle cells groups _ cellGroups)
  = groupSet @=? cellGroupsSet
  where
    groupSet = unionAll (S.fromList <$> groups)
    cellGroupsSet = unionAll (S.fromList <$> (concat (cellGroups <$> cells)))

    unionAll = foldr S.union S.empty
