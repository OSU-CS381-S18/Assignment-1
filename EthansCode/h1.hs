
-------------------Exercise 1 part a---------------------------------------------------------

data Cmd = Pen Mode 
 | MoveTo Pos Pos 
 | Def String Pars Cmd 
 | Call String Vals 
 | Exc Cmd Cmd
 | Empty

data Mode = Up | Down
data Pos = NumPos Int | NamePos String
data Pars = NamePars String | NameParsList String Pars
data Vals = NumValsList Int Vals | NumVals Int

-------------------Exercise 1 part b---------------------------------------------------------
 
vector = Def "vector" (NameParsList "x1" (NameParsList "y1"(NameParsList "x2" (NamePars "y2"))))
    (Exc (Pen Up) (Exc(MoveTo (NamePos "x1") (NamePos "y1"))(Exc (Pen Down) (MoveTo (NamePos "x2") (NamePos "y2")))))
	
-------------------Exercise 1 part c---------------------------------------------------------

steps :: Int -> Cmd 
steps n
 | n <= 0 = Empty
 | n == 1 = Exc (Pen Up) 
           (Exc (MoveTo (NumPos n) (NumPos n)) 
		   (Exc (Pen Down) (Exc (MoveTo (NumPos (pred n)) (NumPos n)) (MoveTo (NumPos (pred n)) (NumPos (pred n))))))
 | otherwise = Exc 
              (Exc (Pen Up) 
			  (Exc (MoveTo (NumPos n) (NumPos n)) 
			  (Exc (Pen Down) 
			  (Exc (MoveTo (NumPos (pred n)) (NumPos n)) (MoveTo (NumPos (pred n)) (NumPos (pred n))))))) (steps (pred n))
			  
-------------------Exercise 2 part a---------------------------------------------------------

data Circuit = MakeCircuit Gates Links

data Gates = GateVal Int GateFn Gates | EmptyGate
data GateFn = And
            | Or
			| Xor
			| Not

data Links = Connect (Int,Int) (Int,Int) Links | EmptyLink

-------------------Exercise 2 part b---------------------------------------------------------

halfAdder = MakeCircuit (GateVal (1) (Xor) (GateVal (2) (And) (EmptyGate))) (Connect (1,1)(2,1) (Connect(1,2) (2,2) (EmptyLink)))

-------------------Exercise 2 part c---------------------------------------------------------

-------------------Exercise 3 part a---------------------------------------------------------
-- (-(3+4)*7)
data Expr = N Int
          | Plus Expr Expr
		  | Times Expr Expr
		  | Neg Expr

data Op = Add | Multiply | Negate

data Exp = Num Int
         | Apply Op [Exp]

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

translate :: Expr -> Exp
translate (N x) = (Num x)
translate (Pluse x y) = Apply Add [translate x, translate y]
translate (Times x y) = Apply Multiply [translate x, translate y]
translate (Neg x) = Apply Negate [translate x]