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
data Mode = Up 
    | Down
data Pos = NumPos Int 
    | NamePos String
data Pars = NamePars String
    | NameParsRecur String Pars
data Vals = NumVals Int
    | NumValsRecur Int Vals


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
--type Circuit = Circ (Gates, Links) -- paths will lead to a gate or a link
--type Gates = GT [(Int,Gate)] -- creates a list of the actions of hi or low to gate
--data Gate = And | Or | Xor | Not -- gate types
--type Links = [Link] -- list of links
--data Link  = FromTo (Int, Int) (Int, Int) -- input , output



data Circuit = Circuits Gates LinkLinks
data Gates = HiLow Int Gate Gates | None
data Gate = And | Or | Xor | Not
data LinkLinks = L Link Link LinkLinks | MT
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
        None))
    (L (1, 1) (2, 1) 
    (L (1, 2) (2, 2) 
        MT))

-- Exercise 2. DiCiDL program 
-- c)
printGate :: Gate -> String
printGate And = "and"
printGate Or  = "or"
printGate Xor = "xor"
printGate Not = "not"
printLinkLinks :: LinkLinks -> String
printLinkLinks MT = ""
printLinkLinks (L (g1, w1) (g2, w2) rest)
            = "from " ++ show g1 ++ "." ++ show w1 ++ " to " 
            ++ show g2 ++ "." ++ show w2 ++ "; " ++ printLinkLinks rest
printGates :: Gates -> String
printGates None = ""
printGates (HiLow g gf rest) 
            = show g ++ ":" ++ printGate gf ++ "; " ++ printGates rest
printCircuit :: Circuit -> String
printCircuit (Circuits gates links) = printGates gates ++ printLinkLinks links




{-
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

prettyCircuit :: Circuit -> String
prettyCircuit (MakeCircuit x y) = prettyGates x++" "++prettyLinks y

prettyGates :: Gates -> String
prettyGates (EmptyGate) = ""
prettyGates (GateVal x y z) = show x++"("++prettyFn y++")" ++ prettyGates z

prettyLinks :: Links -> String
prettyLinks (EmptyLink) = ""
prettyLinks (Connect (x1, y1) (x2,y2) z) = "Gate connects from "++"("++show x1++"."++show y1++ ") to ("++show x2++"."++show y2++")"

prettyFn :: GateFn -> String
prettyFn And = "And"
prettyFn Or = "Or"
prettyFn Not = "Not"
prettyFn Xor = "Xor"
  -}  





















