-- Homework 1, CS 381 Spring 2018
-- Sam Ullrich

--  Exercise 1. Mini Logo
{-
cmd ::= pen mode
    | moveto (pos,pos)
    | def name ( pars ) cmd
    | call name ( vals )
    | cmd; cmd
mode ::= up | down
pos ::= num | name
pars ::= name, pars | name
vals ::= num, vals | num
-}

-- Exercise 1. Mini Logo 
-- a) 
data Cmd = Pen Mode
         | MoveTo Pos Pos
         | Def String Pars Cmd
         | Call String Vals
         | Exp Cmd Cmd
         | NoAction
         deriving (Show)
data Mode = Up 
    | Down
    deriving (Show)
data Pos = NumPos Int 
    | NamePos String
    deriving (Show)
data Pars = NamePars String
    | NameParsRecur String Pars
    deriving (Show)
data Vals = NumVals Int
    | NumValsRecur Int Vals
    deriving (Show)

-- Exercise 1. Mini Logo 
-- b) 
vector :: Cmd
vector = Def "vector" (
    NameParsRecur "x1" (
    NameParsRecur "y1" (
    NameParsRecur "x2" (
    NamePars      "y2" ))))
    (Exp (Pen Up)               
        (Exp (MoveTo (NamePos "x1") 
             (NamePos "y1")) 
        (Exp (Pen Down)                             
             (MoveTo (NamePos "x2") 
                 (NamePos "y2")))))

-- Exercise 1. Mini Logo 
-- c)
steps :: Int -> Cmd
steps n
      | n <= 0 = Exp (Pen Up) 
      (Exp (MoveTo (NumPos n) (NumPos n)) 
         (Pen Down))
      | n == 1 = Exp (Pen Up) 
      (Exp (MoveTo (NumPos n) (NumPos n)) 
        (Exp (Pen Down) 
            (Exp (MoveTo (NumPos (pred n)) 
               (NumPos n)) 
            (MoveTo (NumPos (pred n)) 
               (NumPos (pred n))))))
      | otherwise = Exp (Exp (Pen Up) 
        (Exp (MoveTo (NumPos n) (NumPos n)) 
            (Exp (Pen Down) (Exp (MoveTo (NumPos (pred n)) 
                (NumPos n)) (MoveTo (NumPos (pred n))    
                (NumPos (pred n))))))) 
        (steps (pred n))

-- Exercise 2. DiCiDL program 
{-
circuit ::= gates links
gates ::= num:gateFn ; gates | ε
gateFn ::= and |or |xor |not
link ::= from num.num to num.num; links | 
-}

-- Exercise 2. DiCiDL program 
-- a)
data Circuit = Circuits Gates LinkLinks
data Gates = HiLow Int Gate Gates | EmptyGate
data Gate = And | Or | Xor | Not
data LinkLinks = L Link Link LinkLinks | EmptyLink
type Link = (Int, Int)

-- Exercise 2. DiCiDL program 
-- b)

{-
1:xor;
2:and;
from 1.1 to 2.1;
from 1.2 to 2.2;
-}
halfadder = Circuits 
    (HiLow 1 Xor 
    (HiLow 2 And 
        EmptyGate))
    (L (1, 1) (2, 1) 
    (L (1, 2) (2, 2) 
        EmptyLink))

-- Exercise 2. DiCiDL program 
-- c)
-- input to print: printCircuit halfadder
printGate :: Gate -> String
printGate And = "and"
printGate Or  = "or"
printGate Xor = "xor"
printGate Not = "not"
printLinkLinks :: LinkLinks -> String
printLinkLinks EmptyLink = ""
printLinkLinks (L (g1, w1) (g2, w2) rest)
            = "from " ++ show g1 ++ "." ++ show w1 ++ " to " 
            ++ show g2 ++ "." ++ show w2 ++ "; " ++ printLinkLinks rest
printGates :: Gates -> String
printGates EmptyGate = ""
printGates (HiLow g gf rest) 
            = show g ++ ":" ++ printGate gf ++ "; " ++ printGates rest
printCircuit :: Circuit -> String
printCircuit (Circuits gates links) = printGates gates ++ printLinkLinks links

-- Exercise 3. Designing Abstract Syntax
{-
Abstract syntax 1

data Expr = N Int
    | Plus Expr Expr
    | Times Expr Expr
    | Neg Expr


Abstract syntax 2

data Op = Add | Multiply | Negate
    data Exp = Num Int
        | Apply Op [Exp]
-}

-- Exercise 3. Designing Abstract Syntax
-- a)

data Expr = N Int
          | Plus Expr Expr
          | Times Expr Expr
          | Neg Expr
          deriving Show

data Op = Add
        | Multiply
        | Negate
        deriving Show

data Exp = Num Int
         | Apply Op [Exp]
         deriving Show

exp = Apply Multiply [ Apply Negate [ Apply Add [ Num 3, Num 4 ] ], Num 7]

-- Exercise 3. Designing Abstract Syntax
-- b)

{-
It apears that 'Abstract syntax 2' can take a list of expressions.
Abstract syntax 1 can only take two exressions as input.
-}


-- Exercise 3. Designing Abstract Syntax
-- c)
translate :: Expr -> Exp
translate (N x) = (Num x)
translate (Plus x y) = Apply Add [translate x, translate y]
translate (Times x y) = Apply Multiply [translate x, translate y]
translate (Neg x) = Apply Negate [translate x]



























