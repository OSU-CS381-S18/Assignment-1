-- Homework 1, CS 381 Spring 2018
-- Lucien Tamno

--  Exercise 1. Mini Logo

--  a)

data Cmd = Pen Mode
		 | MoveTo Pos Pos
		 | Def String Pars Cmd
		 | Call String Vals
		 | Next Cmd Cmd
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
		   
-- b)

vector :: Cmd
vector = Def "vector" (-- specify parameters
					  NameParsRecur "x1" (
					  NameParsRecur "y1" (
					  NameParsRecur "x2" (
					   NamePars      "y2" )))
					  )
					  (-- specify functionality
					   Next (Pen Up)                               (
					   Next (MoveTo (NamePos "x1") (NamePos "y1")) (
					   Next (Pen Down)                             (
					   MoveTo (NamePos "x2") (NamePos "y2")       )))
					  )

-- c)
steps :: Int -> Cmd
steps n 
		| n <= 0 = (Next (Pen Up) 
				   (Next (MoveTo (NumPos n) (NumPos n)) 
				   	(Pen Down)))
-- here is the patch from the initial file we don't need this step and correct the (otherwise) case
	  	| otherwise = (Next  (Pen Up) 
	  				  (Next (MoveTo (NumPos n) (NumPos n)) 
	  				  (Next (Pen Down) 
	  				  (Next (MoveTo (NumPos(pred n)) (NumPos n)) 
	  				  (Next (MoveTo (NumPos(pred n))(NumPos(pred n))) 
	  				  (steps (pred n)) )))))


-- Exercise 2 Digital Circuit Design Language
-- 
data Circuit = Cr Gates Links

data Gates = NumG Int GateFn Gates
			| EmptyG

data GateFn = And | Or | Xor | Not

data Links = NumL Int Int Links
			| EmptyL

