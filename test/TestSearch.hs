module TestSearch where

import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec

import Sudoku.Search
import Sudoku.Puzzle
import Sudoku.Puzzle.Standard

test_all :: TestTree
test_all = testGroup "Solutions"
  [ smallSize
  , standardSize
  ]

smallSize :: TestTree
smallSize = testGroup "2x2"
  [ testCase "Empty" (findsSolution smallPuzzle emptyContents)
  ]

standardSize :: TestTree
standardSize = testGroup "9x9"
  [ testCase "Example 1" example1
  ]

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

findsSolution :: Puzzle -> Contents -> Assertion
findsSolution puzzle start = assertBool "No solutions found" (length solutions > 0)
  where
    solutions = solvePuzzle puzzle start

example1 :: Assertion
example1 =
  let
    puzzleInputData
      = concat
      [ "003020600"
      , "900305001"
      , "001806400"
      , "008102900"
      , "700000008"
      , "006708200"
      , "002609500"
      , "800203009"
      , "005010300"
      ]

    puzzleResultData
      = concat
      [ "483921657"
      , "967345821"
      , "251876493"
      , "548132976"
      , "729564138"
      , "136798245"
      , "372689514"
      , "814253769"
      , "695417382"
      ]

    mInputContents = parseMaybe parseContents puzzleInputData
    mResultContents = parseMaybe parseContents puzzleResultData

    check :: Contents -> Contents -> Assertion
    check inputContents resultContents
      = case solvePuzzle mkStandardPuzzle inputContents of
          []     -> assertFailure "No results found"
          (x: _) -> resultContents @=? x

  in case mInputContents of
    Nothing            -> assertFailure "Input data not parsed"
    Just inputContents -> case mResultContents of
      Nothing             -> assertFailure "Result data not parsed"
      Just resultContents -> check inputContents resultContents
