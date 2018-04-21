--Name :: String -> Type
--Num :: Int -> Type

--data Type = Name String | Num Int

--data Cmd = Pen Mode | MoveTo Pos Pos | Def String Pars Cmd -- | Call String Vals

--data Mode = Up | Down
--data Pos = Type | Name String
--data Pars = Pars String | Word String
--data Vals = Vals Int | Int

--data Type = Name String | Num Int

data Cmd = Pen Mode | MoveTo Pos Pos | Def String Pars Cmd | Call String Vals | Cmd Cmd

data Mode = Up | Down
data Pos = Num1 Int | Name1 String
data Pars = String Pars | Name2 String
data Vals = Num2 Int Vals | Num3 Int
