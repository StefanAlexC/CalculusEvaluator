module Calculus where

import Data.Maybe

data UnOp = Neg | Sin | Cos | Log
          deriving (Eq, Ord, Show)

data BinOp = Add | Mul | Div
           deriving (Eq, Ord, Show)

data Exp = Val Double | Id String | UnApp UnOp Exp | BinApp BinOp Exp Exp
         deriving (Eq, Ord, Show)

type Env = [(String, Double)]

functionDatabase :: [(UnOp, Double -> Double)]
functionDatabase = [(Neg, (negate)), (Sin, (sin)), (Cos, (cos)), (Log, (log))]

functionDatabase' :: [(BinOp, Double -> Double -> Double)]
functionDatabase' = [(Add, (+)), (Mul, (*)), (Div, (/))]

lookUp :: Eq a => a -> [(a, b)] -> b
lookUp _ [] 
   = error "search parameter is not a member of the list"
lookUp a ((x, x') : xs) 
   | x == a = x'
   | otherwise = lookUp a xs 

eval :: Exp -> Env -> Double
eval (Val number) database
   = number
eval (Id variable) database
   = fromJust (lookup variable  database)
eval (UnApp function expresion) database
   = fromJust (lookup function functionDatabase) (eval expresion database)
eval (BinApp function expresion expresion') database
   = fromJust (lookup function functionDatabase') (eval expresion database) (eval expresion' database)

diff :: Exp -> String -> Exp
diff (Val number) refrence
   = Val 0.0
diff (Id variable) refrence
   | variable  == refrence = Val 1.0
   | otherwise             = Val 0.0
diff (UnApp function expresion) refrence
   | function  == Sin = BinApp Mul (UnApp Cos expresion) (diff expresion refrence)
   | function  == Cos = UnApp Neg (BinApp Mul (UnApp Sin expresion) (diff expresion refrence))
   | function  == Log = BinApp Div (diff expresion refrence) expresion
   | otherwise        = UnApp Neg (diff expresion refrence)
diff (BinApp function expresion expresion') refrence
   | function  == Add = BinApp Add (diff expresion refrence) (diff expresion' refrence)
   | function  == Mul = BinApp Add (BinApp Mul (expresion) (diff expresion' refrence)) (BinApp Mul (diff expresion refrence) (expresion'))
   | otherwise        = BinApp Div (BinApp Add (BinApp Mul (diff expresion refrence) (expresion')) (UnApp Neg (BinApp Mul (expresion) (diff expresion' refrence)))) (BinApp Mul expresion' expresion')

diff' :: (Exp, String) -> (Exp, String)
diff' (expresion, refrence) = (diff expresion refrence, refrence)

--Add f(0) !!!
maclaurin :: Exp -> Double -> Int -> Double
maclaurin expresion x n 
   = foldl (+) 0.0 terms
   where
      terms = zipWith3 (nterm) (listDeriv) (take (n) (scanl (*) 1 [1, 2..])) (take (n) (scanl (*) 1 [x, x..]))
      (listDeriv, _) = unzip (take (n) (iterate diff' (expresion, "x")))
      nterm :: Exp -> Double -> Double -> Double  
      nterm expresion factorial power = (eval expresion [("x", 0)]) * power / factorial 

stringUnOp :: UnOp -> String
stringUnOp function
   | function == Sin = "sin("
   | function == Cos = "cos("
   | function == Log = "log("
   | function == Neg = "-("

showExp :: Exp -> String
showExp (Val number)
   = show number
showExp (Id variable)
   = variable
showExp (UnApp function expresion)
   = foldr (++) "" [stringUnOp function, showExp expresion, ")"]   
showExp (BinApp Add expresion expresion')
   | showExp expresion  == "0" = showExp expresion'
   | showExp expresion' == "0" = showExp expresion
   | otherwise = foldr (++) "" ["(", showExp expresion, "+", showExp expresion', ")"] 
showExp (BinApp Mul expresion expresion')
   | showExp expresion  == "1" = showExp expresion'
   | showExp expresion' == "1" = showExp expresion
   | otherwise = foldr (++) "" ["(", showExp expresion, "*", showExp expresion', ")"]
showExp (BinApp Div expresion expresion')
   = foldr (++) "" ["(", showExp expresion, "/", showExp expresion', ")"]
   

---------------------------------------------------------------------------
-- Test cases from the spec.

e1, e2, e3, e4, e5, e6 :: Exp

-- > 5*x
e1 = BinApp Mul (Val 5.0) (Id "x")

-- > x*x + y - 7
e2 = BinApp Add (BinApp Add (BinApp Mul (Id "x") (Id "x")) (Id "y"))
                (UnApp Neg (Val 7.0))

-- > x-y^2/(4*x*y-y^2)::Exp
e3 = BinApp Add (Id "x")
            (UnApp Neg (BinApp Div (BinApp Mul (Id "y") (Id "y"))
            (BinApp Add (BinApp Mul (BinApp Mul (Val 4.0) (Id "x")) (Id "y"))
                        (UnApp Neg (BinApp Mul (Id "y") (Id "y"))))))

-- > -cos x::Exp
e4 = UnApp Neg (UnApp Cos (Id "x"))

-- > sin (1+log(2*x))::Exp
e5 = UnApp Sin (BinApp Add (Val 1.0)
                           (UnApp Log (BinApp Mul (Val 2.0) (Id "x"))))

-- > log(3*x^2+2)::Exp
e6 = UnApp Log (BinApp Add (BinApp Mul (Val 3.0) (BinApp Mul (Id "x") (Id "x")))
                           (Val 2.0))

----------------------------------------------------------------------
-- EXTENSION: Uncomment and complete these...

-- instance Num Exp where

-- instance Fractional Exp where

-- instance Floating Exp where


-- instance (Eq a, Num a) => Num (Maybe a) where

-- instance (Eq a, Fractional a) => Fractional (Maybe a) where

-- diff2 :: Exp -> String -> Maybe Exp



-- The following makes it much easier to input expressions, e.g. sin x, log(x*x) etc.

x, y :: Exp
x = Id "x"
y = Id "y"
