{-
- Names:
-  Daniel Domme
-  Blake Hudson
-  Nickoli Londura
-  Ethan Patterson
-}
-------------------Exercise 1 part a---------------------------------------------------------
-- Cmd defines new data type with constructers for Mini Logo
data Cmd = Pen Mode 
         | MoveTo Pos Pos 
         | Def String Pars Cmd 
         | Call String Vals 
         | Exc Cmd Cmd 
         | Empty
 deriving Show -- Using Show to print out vector

data Mode = Up | Down
 deriving Show -- Using Show to print out vector
data Pos = NumPos Int | NamePos String
 deriving Show -- Using Show to print out vector
data Pars = NamePars String | NameParsList String Pars
 deriving Show -- Using Show to print out vector
data Vals = NumValsList Int Vals | NumVals Int
 deriving Show -- Using Show to print out vector

-------------------Exercise 1 part b---------------------------------------------------------
 -- Def takes a String Pars and lastly a Cmd.
 -- First we define the posisiton that will be used than we pickup the pen move it to the first coordanate then we put the
 -- pen down and drage it to the second point.
vector = Def "vector" (NameParsList "x1" (NameParsList "y1"(NameParsList "x2" (NamePars "y2"))))
    (Exc (Pen Up) (Exc(MoveTo (NamePos "x1") (NamePos "y1"))(Exc (Pen Down) (MoveTo (NamePos "x2") (NamePos "y2")))))
    
-------------------Exercise 1 part c---------------------------------------------------------
-- Steps will draw a series of n steps backwords.
steps :: Int -> Cmd 
steps n
 | n <= 0 = Empty -- Base case for when n reaches zero.
 | otherwise = Exc 
              (Exc (Pen Up) 
              (Exc (MoveTo (NumPos n) (NumPos n)) 
              (Exc (Pen Down) 
              (Exc (MoveTo (NumPos (pred n)) (NumPos n)) (MoveTo (NumPos (pred n)) (NumPos (pred n))))))) (steps (pred n))
              
-------------------Exercise 2 part a---------------------------------------------------------

data Circuit = MakeCircuit Gates Links -- Defines a Circuit
data Gates = GateVal Int GateFn Gates  -- Defines a Gate tyep 
           | EmptyGate 
           
data GateFn = And -- Defines gate frequency.
            | Or
            | Xor
            | Not

data Links = Connect (Int,Int) (Int,Int) Links  -- Defines what gate links to what.
           | EmptyLink
           
-------------------Exercise 2 part b---------------------------------------------------------
-- Defines a halfkAdder circuit. First arg is what gate and the second is who connects to who.
halfAdder = MakeCircuit (GateVal (1) (Xor) (GateVal (2) (And) (EmptyGate))) (Connect (1,1)(2,1) (Connect(1,2) (2,2) (EmptyLink)))

-------------------Exercise 2 part c---------------------------------------------------------
-- The pretty function for circuit takes the the gate for x and the link for y.
prettyCircuit :: Circuit -> String
prettyCircuit (MakeCircuit x y) = prettyGates x++" "++prettyLinks y
-- GateVall takes an int for x a gateFn for y and a recursive call for z.
prettyGates :: Gates -> String
prettyGates (EmptyGate) = ""
prettyGates (GateVal x y z) = show x++":("++prettyFn y++");"++ "\n" ++ prettyGates z
-- Connect takes two tuples of ints which are the the circuit connections. z is the recursive call for connect.
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

data Op = Add 
        | Multiply 
        | Negate
    deriving Show

data Exp = Num Int 
         | Apply Op [Exp]
 deriving Show

a =  Neg (Times (Plus (N 3) (N 4)) (N 7))
b = Apply Negate [Apply Multiply [Apply Add [Num 3, Num 4],Num 7]]

-------------------Exercise 3 part b---------------------------------------------------------
    
{-The benefit of using the Expr representation is that it is clear what the constructors expect, for 
example, Neg can only take one argument. This is not so in the Exp representation, the arithmetic operations can receive multiple
arguments. Its disadvantage may also be due to taking a predefined number of arguments such as the case of multiplying three numbers requires 
two multiplication operations.
 
The Exp representation does a better job of encapsulation, using different types to reduce the overhead of a single representation. Its 
disadvantage is that OP does not know how many arguments to expect, such as the case for Neg receiving multiple arguments, this 
could be an issue. On the other hand, however, this could be benefitable, we could now multiply three numbers with one operation.
-}
    
-------------------Exercise 3 part c---------------------------------------------------------

translate :: Expr -> Exp
translate (N x) = (Num x)
translate (Plus x y) = Apply Add [translate x, translate y]
translate (Times x y) = Apply Multiply [translate x, translate y]
translate (Neg x) = Apply Negate [translate x]