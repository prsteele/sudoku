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
  [ testCase "Empty puzzle" (findsSolution smallPuzzle emptyContents)
  ]

standardSize :: TestTree
standardSize = testGroup "9x9"
  [ testCase "Empty puzzle" (findsSolution mkStandardPuzzle emptyContents)
  , testCase "Example 1" example1
  , testCase "Example 2" example2
  ]

smallPuzzle :: Puzzle
smallPuzzle
  = Puzzle
  { _puzzleCells = curry Cell <$> [0, 1] <*> [0, 1]
  , _puzzleGroups = fmap (Group . fmap Cell)
                    [ [(0, 0), (1, 0)]
                    , [(0, 1), (1, 1)]
                    , [(0, 0), (0, 1)]
                    , [(1, 0), (1, 1)]
                    ]
  , _puzzleAlphabetSize = 2
  , _puzzleCellGroups = defaultCellGroups smallPuzzle
  }

findsSolution :: Puzzle -> Contents -> Assertion
findsSolution puzzle start = assertBool "No solutions found" (not (null solutions))
  where
    solutions = solvePuzzle puzzle start

mkPuzzleAssertion :: String -> String -> Assertion
mkPuzzleAssertion input result
  = case mInputContents of
      Nothing            -> assertFailure "Input data not parsed"
      Just inputContents -> case mResultContents of
        Nothing             -> assertFailure "Result data not parsed"
        Just resultContents -> check inputContents resultContents
  where
    mInputContents = parseMaybe parseContents input
    mResultContents = parseMaybe parseContents result

    check :: Contents -> Contents -> Assertion
    check inputContents resultContents
      = case solvePuzzle mkStandardPuzzle inputContents of
          []     -> assertFailure "No results found"
          (x: _) -> resultContents @=? x

example1Input
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

example1Result
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

example1 :: Assertion
example1 = mkPuzzleAssertion example1Input example1Result

example2Input =
  concat
  [ "200080300"
  , "060070084"
  , "030500209"
  , "000105408"
  , "000000000"
  , "402706000"
  , "301007040"
  , "720040060"
  , "004010003"
  ]

example2Result
  = concat
  [ "245981376"
  , "169273584"
  , "837564219"
  , "976125438"
  , "513498627"
  , "482736951"
  , "391657842"
  , "728349165"
  , "654812793"
  ]

example2 :: Assertion
example2 = mkPuzzleAssertion example2Input example2Result
