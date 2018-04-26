-- Homework 1, CS 381 Spring 2018
-- Taylor Griffin, Lucien Tamno, Sam Ulrich, Ethan Ahuja

--  Exercise 1. Mini Logo

--  a)

data Cmd = Pen Mode
  | MoveTo Pos Pos
  | Def String Pars Cmd
  | Call String Vals
  | Exp Cmd Cmd
  deriving Show

data Mode = Up | Down
  deriving Show

data Pos = NumPos Int
  | NamePos String
  deriving Show

data Pars = NamePars String
  | NameParsRecur String Pars
  deriving Show


data Vals = NumVals Int
  | NumValsRecur Int Vals
  deriving Show

--  b)

vector :: Cmd
vector = Def "vector"
  (-- specify parameters
  NameParsRecur "x1" (
  NameParsRecur "y1" (
  NameParsRecur "x2" (
  NamePars      "y2" )))
  )
  (-- specify functionality
  Exp (Pen Up)                               (
  Exp (MoveTo (NamePos "x1") (NamePos "y1")) (
  Exp (Pen Down)                             (
  MoveTo (NamePos "x2") (NamePos "y2")       )))
  )

--  c)

steps :: Int -> Cmd
steps n
      | n <= 0 = (Exp (Pen Up)
                 (Exp (MoveTo (NumPos n) (NumPos n))
                 (Pen Down)))
      | otherwise = (Exp  (Pen Up)
              (Exp (MoveTo (NumPos n) (NumPos n))
              (Exp (Pen Down)
              (Exp (MoveTo (NumPos(pred n)) (NumPos n))
              (Exp (MoveTo (NumPos(pred n))(NumPos(pred n)))
              (steps (pred n)) )))))

--  Exercise 2. Digital Circuit Design Language

--  a)

data Circuit = Cr Gates Links

data Gates = MyGate Int GateFn Gates
  | EmptyG

data GateFn = And | Or | Xor | Not

data Links = MyLink Link Link Links
  | EmptyL

type Link = (Int,Int)

--  b)

halfadder:: Circuit
halfadder = Cr (MyGate 1 Xor (MyGate 2 And EmptyG))
  (MyLink (1,1)(2,1) (MyLink(1,2)(2,2) EmptyL))

--  c)

print_circuit:: Circuit -> String
print_circuit (Cr thisgate thislink) = "printing" ++ print_gates thisgate ++ print_links thislink

print_gates:: Gates -> String
print_gates EmptyG = ""
print_gates (MyGate number gateOp nextgate) = " circuit-" ++show number ++":" ++ print_Gatefn gateOp ++ ";" ++ print_gates nextgate

print_links:: Links -> String
print_links EmptyL = ""
print_links (MyLink (a,b)(c,d) nextlink) = " "++"from"++" "++ show a ++"."++ show b ++" "++"to"
                                           ++" "++ show c++"." ++ show d ++ " " ++ "...."++ print_links nextlink
print_Gatefn:: GateFn -> String
print_Gatefn Xor = "xor"
print_Gatefn And = "and"
print_Gatefn Not = "not"
print_Gatefn Or = "or"

--  Exercise 3. Designing Abstract Syntax

-- Expr Abstract Syntax
data Expr = N Int
  | Plus Expr Expr
  | Times Expr Expr
  | Neg Expr

-- Exp Abstract Syntax
data Op = Add | Multiply | Negate

data Exp = Num Int
  | Apply Op [Exp]

--  a)

e = Apply Negate [Apply Multiply [Apply Add [Num 3, Num 4], Num 7]]

--  b)

{-

The advantages and disadvantages for the two different abstract syntaxes
mostly lie in two areas: clarity, and ease of use.

The Exp Abstract syntax seems slightly more intuitive and easy to implement,
because you build an expression from the outside -> in, similar to how it's
represented when written down. For example, when representing "-(3+4)*7" in
the Exp syntax, the left most directive was Negate and the rightmost argument
supplied was 7 (similar to how the leftmost character in the string expression is
- and the rightmost character is 7). The Expr Abstact Syntax however builds
from the inside -> out, which is slightly less intuitive.

However, when it comes to clarity, one could argue Expr is much more clear and
is therefore the preferrable syntax to use, since it specifies the number of
arguments to be passed when using Plus, Times, and Neg. This ensures that one
won't accidently pass Plus or Times one argument, or pass Neg two. The Exp syntax
on the other hand, only specifies [Exp] be passed when using the Apply constructor,
which gives no indication how many integers should be used when using Add, Multiply,
or Negate.

-}

--  c)

translate :: Expr -> Exp
translate (N a) = (Num a)
translate (Plus a b) = Apply Add [translate a, translate b]
translate (Times a b) = Apply Multiply [translate a, translate b]
translate (Neg a) = Apply Negate [translate a]
