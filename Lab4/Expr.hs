module Expr where

import Test.QuickCheck
  hiding (Function, function)

import Parsing
import Data.Char
import Data.Maybe
import Control.Monad

-- A
data Expr = Num Double
          | Var
          | Op Operator Expr Expr
          | Func Function Expr
          deriving Eq

{- instance Eq Expr where
  expr1 == expr2 = showExpr expr1 == showExpr expr2
    where showExpr = trimSpace . show
-}
instance Arbitrary Expr where
  arbitrary = sized arbExpr


data Operator = Add | Mul
  deriving Eq

example1 = Num 4                                -- 4
example2 = Var                                  -- x
example3 = Op Add (Num 4) (Num 4)               -- 4 + 4
example4 = Op Mul Var (Num 2)                   -- x * 2
example5 = Func Sin Var                         -- Sin(x)
example6 = Op Add (Func Sin Var) (Func Sin Var) -- Sin(x) + Sin(x)
example7 = Op Add (Func Cos Var) (Func Sin Var) -- Cos(x) + Sin(x)
example8 = Op Mul (Func Cos Var) (Func Cos Var) -- Cos(x) * Cos(x)
example9 = Op Add (Num 4) (Op Mul (Func Sin (Num 3)) (Num 5)) -- 4 + Sin(3) * 5

-- Helper function for getting the right operator
getOp :: (Num a) => Operator -> (a -> a -> a)
getOp Add = (+)
getOp Mul = (*)

data Function = Sin | Cos
  deriving Eq

-- Helper function for getting the right function
getFunc :: (Floating a) => Function -> (a -> a)
getFunc Sin = sin
getFunc Cos = cos

-- B
instance Show Expr where
  show = showExpr

-- Implement own show function
showExpr :: Expr -> String
showExpr (Num n) = show n
showExpr Var = "x"
showExpr (Op operator a b) = "(" ++ showExpr a ++ " "
                             ++ showOp operator ++ " "
                             ++ showExpr b ++ ")"

showExpr (Func function a) = showFunc function ++
                             "( " ++
                             showExpr a ++
                             " )"

-- Override Show function for own operator type
instance Show Operator where
  show = showOp
showOp :: Operator -> String
showOp Add = "+"
showOp Mul = "*"

-- Override Show function for own function type
instance Show Function where
  show = showFunc
showFunc :: Function -> String
showFunc Sin = "Sin"
showFunc Cos = "Cos"

-- C

-- Implement a function for evaulating an Expression
eval :: Expr -> Double -> Double
eval (Op operator a b) x = applyOp (eval a x) (eval b x)
  where applyOp = getOp operator
eval (Func func a) x = applyFunc (eval a x)
  where applyFunc = getFunc func
eval (Num n) _ = n
eval Var x = x


-- D

-- A function for parsing a String to an Expression (Maybe)
readExpr :: String -> Maybe Expr
readExpr s = case parse expr (trimSpace s) of
               Just (e, _) -> Just e
               _ -> Nothing

-- Function for removing white space characters from a string.
trimSpace :: String -> String
trimSpace = filter (not . isSpace)


-- The code below is heavily inspired by lecture notes on parsing (week 4) --
-- * A more elegant expression parser
{- EBNF:
expr   ::= term {"+" term}.
term   ::= factor {"*" factor}.
factor ::= number | "(" expr ")".
-}

expr, term, factor :: Parser Expr

expr = leftAssoc (Op Add) term (char '+')

term = leftAssoc (Op Mul) factor (char '*')

-- Factor states the order in which we want to parse the string. double and var is interchangable, since they both only represent a number.
factor =  functionExpr <|> double <|> parseVar <|> innerExpr
double :: Parser Expr
double = Num <$> readsP

-- Parse for variables. Since 'x' is the only variable allowed, simply return Var if 'x' is succesfully parsed.
parseVar :: Parser Expr
parseVar = do char 'x'
              return Var

-- Parser for functions. Try to parse a sequence of characters that correspond to a function. If successful, return the parsed function.
function :: Parser Function
function = parseSin <|> parseCos

parseSin :: Parser Function
parseSin = do char 'S' <|> char 's'
              char 'I' <|> char 'i'
              char 'N' <|> char 'n'
              return Sin

parseCos :: Parser Function
parseCos = do char 'C' <|> char 'c'
              char 'O' <|> char 'o'
              char 'S' <|> char 's'
              return Cos

-- Parser for an expression sorrounded by parenthesis. We don't really care about the parenthesis, so we return just an expression expr.
innerExpr :: Parser Expr
innerExpr = char '(' *> expr <* char ')'


-- From Week4 Lecture on Parsing page 25
leftAssoc :: (t->t->t) -> Parser t -> Parser sep -> Parser t
leftAssoc op item sep = do is <- chain item sep
                           return (foldl1 op is)

-- Parser for a function expression. If successful, returns a function and the expression it is being called on.
functionExpr :: Parser Expr
functionExpr = do func <- function
                  Func func <$> factor


-- We have defined our own equallity which checks if the value of two expressions are the same, that they are equivalent to each other.
prop_ShowReadExpr :: Expr -> Bool
prop_ShowReadExpr expr = fromJust (readExpr (showExpr expr)) == expr


-- Function for recursivly generating an arbitrary expression.
arbExpr :: Int -> Gen Expr
arbExpr 0 = rExpr -- Base case, simply return a random expression
arbExpr size = do expr1 <- rExpr
                  op    <- rOp
                  expr2 <- arbExpr (size-1)
                  return (Op op expr1 expr2)


-- Function for generating a random expressions. Here, frequency can be tuned for the different types of expressions.
rExpr :: Gen Expr
rExpr = frequency [(5, rNumExpr), (1,rFuncExpr), (3,rOpExpr)]

-- Generate a number expression with value ranging from 1 to 100
rNumExpr :: Gen Expr
rNumExpr = elements [Num n | n <- [1..100]]

-- Generate a function expression
rFuncExpr :: Gen Expr
rFuncExpr = do func <- rFunc
               Func func <$> rExpr

-- Generate a function expression
rOpExpr :: Gen Expr
rOpExpr = do op <- rOp
             expr1 <- rExpr
             Op op expr1 <$> rExpr

-- Generate a random Funcion and Operator
-- Simply choose one or the other. Could be implemented in a nicer way if Function/Operation derived Enum.
rFunc :: Gen Function
rFunc = elements [Sin, Cos]

rOp :: Gen Operator
rOp = elements [Add, Mul]


-- F

-- Function for simplifying an expression. Basically, there are two interesting cases.
-- 1. The expression is an operation expression.
-- 2. The experssion is a function experssion.
simplify :: Expr -> Expr
--
simplify (Op op expr1 expr2) = simplify' (Op op
                                         (simplify expr1)
                                         (simplify expr2))
--
simplify (Func func expr) = simplify' (Func func (simplify expr))

simplify expression = simplify' expression

simplify' :: Expr -> Expr
-- Simplify a Num expressions to itself
simplify' num@(Num _) = num
-- Simplify an addition expression
simplify' (Op Add expr (Num 0)) = simplify' expr
simplify' (Op Add (Num 0) expr) = simplify' expr
simplify' (Op Add (Num x) (Num y)) = (Num (x + y))
simplify' (Op Add expr1 expr2) | expr1 == expr2 = (Op Mul
                                                  (Num 2)
                                                  (simplify' expr1))

                               | otherwise      = (Op Add
                                                  (simplify' expr1)
                                                  (simplify' expr2))

-- Simplify a multiplication expression
simplify' (Op Mul expr (Num 0)) = Num 0
simplify' (Op Mul (Num 0) expr) = Num 0
simplify' (Op Mul expr (Num 1)) = simplify' expr
simplify' (Op Mul (Num 1) expr) = simplify' expr
simplify' (Op Mul (Num x) (Num y)) = (Num (x * y))

simplify' (Op Mul (Num x) (Op Mul (Num y) expr))
  | (x < 0) && (y < 0) = simplify' (Op Mul
                                   (Num (abs x))
                                   (Op Mul (Num (abs y)) expr))

simplify' (Op Mul expr1 expr2)  = (Op Mul
                                  (simplify' expr1)
                                  (simplify' expr2))


simplify' expression = expression


-- Property that checks that simplify is correct
-- A simplified expression should not change/lose information

prop_simplifyCorrectness :: Expr -> Double -> Bool
prop_simplifyCorrectness expr d = eval expr d == eval (simplify expr) d

-- Operator for checking if two values are approximidietally the same.
(==~) :: (Fractional a, Ord a) => a -> a -> Bool
(==~) a b = (abs(1 - (abs(a / b))) < 0.0001)


-- TEESTING
testCalc :: String -> Double -> Double
testCalc expr x = eval(fromJust(readExpr expr)) x

testCalcSimplified :: String -> Double -> Double
testCalcSimplified expr x = eval(simplify(fromJust(readExpr expr))) x


-- G
-- Function which differeniates and simplifies an epxression.
differeniate :: Expr -> Expr
differeniate = simplify . differeniate'

-- Helper function which differeniates and expression
differeniate' :: Expr -> Expr
-- c' = 0
differeniate' (Num _) = Num 0
-- x' = 1
differeniate' Var     = Num 1

-- (c*x)' = c
differeniate' (Op Mul (Num c) Var) = Num c
-- (x*c)' = c
differeniate' (Op Mul Var (Num c)) = Num c

-- (expr1 + expr2)' = expr1' + expr2'
differeniate' (Op Add expr1 expr2) = (Op Add
                                     (differeniate' expr1)
                                     (differeniate' expr2))
-- (expr1 * expr2)' = (expr1' * expr2) + (expr1 + expr2')
differeniate' (Op Mul expr1 expr2) = (Op Add
                                     (Op Mul (differeniate' expr1) expr2)
                                     (Op Mul expr1 (differeniate' expr2)))
-- Sin(expr)' = expr' * Cos(expr)
differeniate' (Func Sin expr) = (Op Mul
                                (differeniate' expr)
                                (Func Cos expr))
-- Cos(expr)' = expr' * -1 * Sin(expr)
differeniate' (Func Cos expr) = (Op Mul (differeniate' expr))
                                (Op Mul (Num (-1)) (Func Sin expr))
