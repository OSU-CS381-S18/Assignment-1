{-
- Names:
-  Daniel Domme
-  Blake Hudson
-  Nickoli Londura
-  Ethan Patterson
-}
-------------------Exercise 1 part a---------------------------------------------------------

data Cmd = Pen Mode 
 | MoveTo Pos Pos 
 | Def String Pars Cmd 
 | Call String Vals 
 | Exc Cmd Cmd
 | Empty
 deriving Show

data Mode = Up | Down
 deriving Show
data Pos = NumPos Int | NamePos String
 deriving Show
data Pars = NamePars String | NameParsList String Pars
 deriving Show
data Vals = NumValsList Int Vals | NumVals Int
 deriving Show

-------------------Exercise 1 part b---------------------------------------------------------
 
vector = Def "vector" (NameParsList "x1" (NameParsList "y1"(NameParsList "x2" (NamePars "y2"))))
    (Exc (Pen Up) (Exc(MoveTo (NamePos "x1") (NamePos "y1"))(Exc (Pen Down) (MoveTo (NamePos "x2") (NamePos "y2")))))
	
-------------------Exercise 1 part c---------------------------------------------------------

steps :: Int -> Cmd 
steps n
 | n <= 0 = Empty
 | otherwise = Exc 
              (Exc (Pen Up) 
			  (Exc (MoveTo (NumPos n) (NumPos n)) 
			  (Exc (Pen Down) 
			  (Exc (MoveTo (NumPos (pred n)) (NumPos n)) (MoveTo (NumPos (pred n)) (NumPos (pred n))))))) (steps (pred n))
			  
-------------------Exercise 2 part a---------------------------------------------------------

data Circuit = MakeCircuit Gates Links
 deriving Show
data Gates = GateVal Int GateFn Gates | EmptyGate
 deriving Show
data GateFn = And
            | Or
			| Xor
			| Not
 deriving Show
data Links = Connect (Int,Int) (Int,Int) Links | EmptyLink
 deriving Show
-------------------Exercise 2 part b---------------------------------------------------------

halfAdder = MakeCircuit (GateVal (1) (Xor) (GateVal (2) (And) (EmptyGate))) (Connect (1,1)(2,1) (Connect(1,2) (2,2) (EmptyLink)))

-------------------Exercise 2 part c---------------------------------------------------------

prettyCircuit :: Circuit -> String
prettyCircuit (MakeCircuit x y) = prettyGates x++" "++prettyLinks y

prettyGates :: Gates -> String
prettyGates (EmptyGate) = ""
prettyGates (GateVal x y z) = show x++":("++prettyFn y++");"++ "\n" ++ prettyGates z

prettyLinks :: Links -> String
prettyLinks (EmptyLink) = ""
prettyLinks (Connect (x1, y1) (x2,y2) z) = "from "++"("++show x1++"."++show y1++ ") to ("++show x2++"."++show y2++");" ++ "\n" ++ prettyLinks z

prettyFn :: GateFn -> String
prettyFn And = "And"
prettyFn Or = "Or"
prettyFn Not = "Not"
prettyFn Xor = "Xor"

-------------------Exercise 3 part a---------------------------------------------------------
-- (-(3+4)*7)
data Expr = N Int
          | Plus Expr Expr
		  | Times Expr Expr
		  | Neg Expr
		  deriving Show

data Op = Add | Multiply | Negate
	deriving Show

data Exp = Num Int
         | Apply Op [Exp]
		 deriving Show

a = Apply Negate [Apply Multiply [Apply Add [Num 3, Num 4],Num 7]]

-------------------Exercise 3 part b---------------------------------------------------------
	
{-The benefit of using the Expr representation is that it is clear what the constructors expect for 
example Neg can only take one argument, this is not so in the Exp representation, the arithmetic operations can receive multiple
arguments. Its disadvantage may also be due to taking a predefined number of arguments such as the case of multiplying three numbers requires 
two multiplication operations.
 
The Exp representation does a better job of encapsulation, using different types to reduce the overhead of a single representation. Its 
disadvantage is that OP does not know how many arguments to expect such as the case for Neg receiving multiple arguments, this 
could be an issue. On the other hand, however, this could be benefitable, we could now multiply three numbers with one operation.
-}
	
-------------------Exercise 3 part c---------------------------------------------------------

convert :: Expr -> Exp
convert (N x) = (Num x)
convert (Plus x y) = Apply Add [convert x, convert y]
convert (Times x y) = Apply Multiply [convert x, convert y]
convert (Neg x) = Apply Negate [convert x]