module Sudoku where

import Test.QuickCheck
import Data.Char
import Data.List
import Data.Maybe

data Sudoku = Sudoku { rows :: [[Maybe Int]] }
  deriving Show


example :: Sudoku
example =
    Sudoku
      [ [j 3,j 6,n  ,n  ,j 7,j 1,j 2,n  ,n  ]
      , [n  ,j 5,n  ,n  ,n  ,n  ,j 1,j 8,n  ]
      , [n  ,n  ,j 9,j 2,n  ,j 4,j 7,n  ,n  ]
      , [n  ,n  ,n  ,n  ,j 1,j 3,n  ,j 2,j 8]
      , [j 4,n  ,n  ,j 5,n  ,j 2,n  ,n  ,j 9]
      , [j 2,j 7,n  ,j 4,j 6,n  ,n  ,n  ,n  ]
      , [n  ,n  ,j 5,j 3,n  ,j 8,j 9,n  ,n  ]
      , [n  ,j 8,j 3,n  ,n  ,n  ,n  ,j 6,n  ]
      , [n  ,n  ,j 7,j 6,j 9,n  ,n  ,j 4,j 3]
      ]
  where
    n = Nothing
    j = Just

example2 :: Sudoku
example2 =
    Sudoku
    [ [j 1,j 2,j 3,j 4,j 5,j 6,j 7,j 8,j 9  ]
    , [j 4,j 5,j 6,j 7,j 8,j 9,j 1,j 2,j 3  ]
    , [j 7,j 8,j 9,j 1,j 2,j 3,j 4,j 5,j 6  ]
    , [j 2,j 3,j 4,j 5,j 6,j 7,j 8,j 9,j 1  ]
    , [j 5,j 6,j 7,j 8,j 9,j 1,j 2,j 3,j 4  ]
    , [j 8,j 9,j 1,j 2,j 3,j 4,j 5,j 6,j 7  ]
    , [j 3,j 4,j 5,j 6,j 7,j 8,j 9,j 1,j 2  ]
    , [j 6,j 7,j 8,j 9,j 1,j 2,j 3,j 4,j 5  ]
    , [j 9,j 1,j 2,j 3,j 4,j 5,j 6,j 7,j 8  ]
    ]
  where
  n = Nothing
  j = Just

-- A1

-- Functions which returns a 9x9 sudoku filled with Nothing
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku [ x | _ <- [1..9], let x = allBlankList 9]


-- Function which creates a list of given length
allBlankList :: Integer -> [Maybe Int]
allBlankList len = [ Nothing | _ <- [1..len]]

-- A2
-- Function which checks if a soduku has 9 rows and 9 columns
isSudoku :: Sudoku -> Bool
isSudoku sudoku = length allRows == 9 && -- Correct no of rows
                       all hasCorrectLen allRows && --Cor no of elem
                       all isValidRow allRows -- Valid elements
                       where allRows = rows sudoku

-- Function that checks if a Sudoku has 9 rows and 9 columns
hasCorrectLen :: [Maybe Int] -> Bool
hasCorrectLen row = length row == 9

-- Function which given a list of Maybe Ints determines if they are valid in Sudoku
isValidRow :: [Maybe Int] -> Bool
isValidRow = all isValidElement

-- Function which given an Maybe Int checks if it is valid (1..9)
isValidElement :: Maybe Int -> Bool
isValidElement Nothing = True
isValidElement (Just n) | n `elem` [1..9] = True
                        | otherwise = False

isValidElementProps :: Int -> Bool
isValidElementProps n = isValidElement (Just n) == ((1 <= n) && (n <= 9))

-- A3

isFilled :: Sudoku -> Bool
isFilled = not
           . all hasBlankElement
           . rows

-- Function returns true if an element is Empty within a list
hasBlankElement :: [Maybe Int] -> Bool
hasBlankElement [x] = isBlankElement x
hasBlankElement (x:xs) = isBlankElement x || hasBlankElement xs

isBlankElement :: Maybe Int -> Bool
isBlankElement Nothing = True
isBlankElement _ = False

-- B1
-- Given a Sudoku, print each row seperated by new line characters.
printSudoku :: Sudoku -> IO ()
printSudoku sudoku = putStr $ unlines $ map printRow formattedSudoku
  where formattedSudoku = rows sudoku

-- Function which applies printCell to an entire list
printRow :: [Maybe Int] -> String
printRow [x]    = printCell x
printRow (x:xs) = printCell x ++ printRow xs

-- Function formats a Maybe Int to desired output String
printCell :: Maybe Int -> String
printCell Nothing = "."
printCell (Just n) = show n

-- B2
-- Read a Sudoku from a file. Expect the file to be correctly formatted.
readSudoku :: FilePath -> IO Sudoku
readSudoku name = do file <- readFile name
                     let rows = lines file
                     let sud = map digitsToInt' rows
                     return (Sudoku sud)

digitsToInt' :: [Char] -> [Maybe Int]
digitsToInt' [] = error "digitsToInt': Empty String"
digitsToInt' [x] | x == '.' = [Nothing]
                 | otherwise = [Just (digitToInt x)]
digitsToInt' (x:xs) = digitsToInt' [x] ++ digitsToInt' xs


-- C1
-- Function which generates an arbitrary cell
cell :: Gen (Maybe Int)
cell = frequency [(1, rElement), (9, rNothing)]

rElement :: Gen (Maybe Int)
rElement = elements [Just n | n <- [1..9]]

rNothing :: Gen (Maybe a)
rNothing = oneof [return Nothing, return Nothing]

-- C2

-- Make Sudokus an instance of the class Arbitrary. 
instance Arbitrary Sudoku where
  arbitrary =
    do rows <- vectorOf 9 (vectorOf 9 cell)
       return (Sudoku rows)

-- C3
prop_Sudoku :: Sudoku -> Bool
prop_Sudoku = isSudoku

-- D1

type Block = [Maybe Int]

-- Function which transposes all the cells in a Sudoku
transposeSudoku :: Sudoku -> Sudoku
transposeSudoku s = Sudoku (transpose allrows)
  where allrows = rows s

-- Function which removes cells with Nothing from a Block.
trim :: Block -> Block
trim [x] | isNothing x = []
         | otherwise = [x]
trim (x:xs) = trim [x] ++ trim xs

-- Function which checks for duplicates in a Block (excluding Nothing).
isOkayBlock :: Block -> Bool
isOkayBlock block = (length trimmedBlock) == (length $ nub trimmedBlock)
  where trimmedBlock = trim block

-- D2

-- Given a Sudoku, creates a list of all blocks of that Sudoku.
blocks :: Sudoku -> [Block]
blocks s = rows s ++
           rows (transposeSudoku s) ++
           [takeBlock n s | n <- [0..8]]


{- Algorithm:
1. Split a Sudoku into regions of 3 horizontal lines.
2. Drop regions until the 3 desired lines are at the top.
3. Take them, transponse them and repeat step 1-2.
-}
takeBlock :: Integer -> Sudoku -> Block
takeBlock n s = concat $
                  selectBlock (transpose (selectBlock sud (fixIndex n))) n
                where sud = rows s
                      mod3 = n `mod` 3

-- Step 2 of above algorithm.
selectBlock :: [[Maybe Int]] -> Integer -> [[Maybe Int]]
selectBlock block n = take 3 (drop mod3 block)
  where mod3 = fromIntegral((n `mod` 3) * 3)


-- Helperfunction for picking the correct region.
fixIndex :: Integer -> Integer
fixIndex n   | n < 3 = 0
             | n < 6 = 1
             | n < 9 = 2

-- D3
isOkay :: Sudoku -> Bool
isOkay s= all isOkayBlock allblocks && isSudoku s
    where allblocks = blocks s


type Pos = (Int,Int)

-- E1
-- Returns all the blank positions in a sudoku
blanks :: Sudoku -> [Pos]
blanks s = blanks' allRows 0
           where allRows = rows s

blanks' :: [[Maybe Int]] -> Int -> [Pos]
blanks' [x] n = blanksInRow x n
blanks' (x:xs) n = blanksInRow x n ++ blanks' xs (n+1)

blanksInRow :: [Maybe Int] -> Int -> [Pos]
blanksInRow row index = [(index, n) | n <- [0..8], isBlankElement (row !! n)]

-- Property for blanks
prop_blanks_allBlank :: Sudoku -> Bool
prop_blanks_allBlank s = and (prop_blanks_allBlank' bls allRows)
                        where bls = blanks s
                              allRows = rows s

prop_blanks_allBlank' :: [Pos] -> [[Maybe Int]] -> [Bool]
prop_blanks_allBlank' [x] s = posNothing x s : []
prop_blanks_allBlank' (x:xs) s = posNothing x s : prop_blanks_allBlank' xs s


posNothing :: Pos -> [[Maybe Int]] -> Bool
posNothing p s = isNothing (s !! fst p !! snd p)

-- E2

-- Given an List of A and a tuple of an index of type Int and a new value of type A inserts the new value while replacing
-- the old value at the given index.
-- Using recursion we use pattern matching until the index is zero:
-- 1. If the index is not zero, we move forward in the list and decrement index by 1,
-- meaning we are 1 step closer to the actual index.
-- 2. When index is zero we add the new value to the list and prepend entire first part of list to second part.
-- Edge case is: If we reached the end of the list, check that index is 0, if so insert newvalue.
(!!=) :: [a] -> (Int, a) -> [a]
(!!=) [] _ = []
(!!=) [x] (index, newValue) | index == 0 = [newValue]
                            | otherwise = [x]
(!!=) (_:xs) (0, newValue) = newValue : xs -- Since x is to be replaced, just trash it
(!!=) (x:xs) (index, newValue) = x : xs !!= (index-1, newValue)

-- Property for correctness of operator (!!=)
prop_bangBangEquals_correct :: Eq a => [a] -> (Int, a) -> Property
prop_bangBangEquals_correct list (index, newValue) = (not . null) list &&
                                                     (index < length list) &&
                                                     (index >= 0) ==>
                                                     (length list == length newList) &&
                                                     (newList !! index == newValue)
  where newList = list !!= (index, newValue)

-- E3
-- After deconstructing given sudoku into lists, we apply "!!=" twice, replacing a list with a new 
-- list with an updated value
update :: Sudoku -> Pos -> Maybe Int -> Sudoku
update sud (row, col) nw = Sudoku (allRows !!= (row, (myRow !!= (col, nw))))
  where allRows = rows sud
        myRow = allRows !! row

-- Property for update
prop_update_updated :: Sudoku -> Pos -> Maybe Int -> Property
prop_update_updated sud (row,col) nw =
                                       isOkay sud &&
                                       nw `elem` ([Just n | n <- [1..8]] ++ [Nothing])
                                       ==>
                                       nw == (nSudRow !! row') !! col'
    where nSudRow = rows nSud
          nSud = update sud (row',col') nw
          row' = row `mod` (8+1)
          col' = col `mod` (8+1)


-- E4
-- Returns possible numbers given a Sudoku and a position
candidates :: Sudoku -> Pos -> [Int]
candidates s (row,col) = [n | n <- [1..9], n `notElem` notPossible]
    where notPossible = catMaybes $ nub $ candidates' s (row, col)

candidates' :: Sudoku -> Pos -> [Maybe Int]
candidates' s (row,col) = sud !! row  ++
                          (transpose sud !! col) ++
                          takeBlock (mapRegion (row,col)) s
    where sud = rows s

prop_candidates_correct :: Sudoku -> Property
prop_candidates_correct sud = isOkay sud && (length $ blanks sud) /= 0 ==>
                              isOkay (update sud pos cand)
                              where allBlanks = blanks sud
                                    pos = head allBlanks
                                    cand = Just (head (candidates sud pos))


-- Given a position returns the sqblock which it is located in
mapRegion :: Pos -> Integer
mapRegion (row, col)
                      | row < 3 && col < 3 = 0
                      | row < 3 && col < 6 = 1
                      | row < 3 && col < 9 = 2
                      | row < 6 && col < 3 = 3
                      | row < 6 && col < 6 = 4
                      | row < 6 && col < 9 = 5
                      | row < 9 && col < 3 = 6
                      | row < 9 && col < 6 = 7
                      | row < 9 && col < 9 = 8


solve :: Sudoku -> Maybe Sudoku
solve s | not (isSudoku s && isOkay s) = Nothing
        | otherwise = solve' s blankPos
        where blankPos = blanks s

solve' :: Sudoku -> [Pos] -> Maybe Sudoku
solve' s [] = Just s -- No blank positions
solve' s [blnk] = solveCell s blnk cands
  where cands = candidates s blnk
solve' s (blnk:_) = solveCell s blnk cands
  where cands = candidates s blnk

  
-- Try first candidate for given cell. If no solution arises, try the next candidate until they are all exhausted.
-- In that case no solution exists to given Sudoku.
solveCell :: Sudoku -> Pos -> [Int] -> Maybe Sudoku
solveCell sud pos cands | null cands = Nothing -- No candidates
                      | isOkay newSud && isSolved newSud = Just newSud -- If current position is okay and soduko is solved, return
                      | isNothing nextSud = solveCell sud pos (tail cands)
                      | otherwise = nextSud
                      where newSud = update sud pos (Just (head cands))
                            nextSud = solve newSud

isSolved :: Sudoku -> Bool
isSolved s = isOkay s  &&
             (all isSolvedBlock allBlocks) &&
             (null $ blanks s)
  where allBlocks = blocks s

isSolvedBlock :: Block -> Bool
isSolvedBlock b = isOkayBlock b && (length (trim b) == 9)


-- F2

readAndSolve :: FilePath -> IO()
readAndSolve file = do sud <- readSudoku file
                       let solved = (solve sud)
                       if(not (isNothing solved))
                         then printSudoku (fromJust solved)
                       else putStrLn "Error solving Sudoku: Nothing"

                       
-- Given two sudokus, compares all the elements. 
-- All the elements in an unfinished sudoku should be the same in finished sudoku
-- Elements should either be equal
-- or
-- One of them should be nothing

isSolutionOf :: Sudoku -> Sudoku -> Bool
isSolutionOf solution sud = isOkay sud
                            && isSolved solution
                            && isSolutionOf' solution' sud'
                        where sud' = concat (rows sud)
                              solution' = concat (rows solution)

isSolutionOf' :: [Maybe Int] -> [Maybe Int] -> Bool
isSolutionOf' [x] [Nothing] = True
isSolutionOf' [x] [y] = x==y
isSolutionOf' (x:xs) (Nothing:ys) = True && isSolutionOf' xs ys
isSolutionOf' (x:xs) (y:ys) = x==y && isSolutionOf' xs ys

-- F4
prop_SolveSound :: Sudoku -> Property
prop_SolveSound sud = not (isNothing proposedSolution) ==>
                      isSolutionOf (fromJust proposedSolution) sud
  where proposedSolution = solve sud
