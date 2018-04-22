-- Homework 1, CS 381 Spring 2018
-- Taylor Griffin

--  Exercise 1. Mini Logo

--  a)

data Cmd = Pen Mode
         | MoveTo Pos Pos
         | Def String Pars Cmd
         | Call String Vals
         | Exp Cmd Cmd

data Mode = Up | Down

data Pos = NumPos Int | NamePos String

data Pars = NamePars String
          | NameParsRecur String Pars

data Vals = NumVals Int
          | NumValsRecur Int Vals

-- b)

vector :: Cmd
vector = Def "vector" (-- specify parameters
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

-- c)
steps :: Int -> Cmd
steps n
      | n <= 0 = Exp (Pen Up) (Exp (MoveTo (NumPos n) (NumPos n)) (Pen Down))
      | n == 1 = Exp (Pen Up) (Exp (MoveTo (NumPos n) (NumPos n)) (Exp (Pen Down) (Exp (MoveTo (NumPos (pred n)) (NumPos n)) (MoveTo (NumPos (pred n)) (NumPos (pred n))))))
      | otherwise = Exp (Exp (Pen Up) (Exp (MoveTo (NumPos n) (NumPos n)) (Exp (Pen Down) (Exp (MoveTo (NumPos (pred n)) (NumPos n)) NumPos (pred n)(MoveTo (NumPos (pred n)) (NumPos (pred n))))))) (steps (pred n))
