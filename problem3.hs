data Expr = N Int
          | Plus Expr Expr
          | Times Expr Expr
          | Neg Expr



data Op = Add | Multiply | Negate
data Exp = Num Int
         | Apply Op [Exp]

-- a)

e = Apply Negate [Apply Multiply [Apply Add [Num 3, Num 4], Num 7]]

-- b)

{-

In the original syntax, creating an expression starts with building the expression in chunks
from the inside out.
It is easier to start with addition, then multiply, and end with a negation, because that's
typically how we evaluate expressions (from the insdie out).

In the alternate syntax, you have to build the expression starting from the outside, which
tends to be more difficult and leads to a sloppy solution.

-}

-- c)

translate :: Expr -> Exp
translate (N a) = (Num a)
translate (Plus a b) = Apply Add [translate a, translate b]
translate (Times a b) = Apply Multiply [translate a, translate b]
translate (Neg a) = Apply Negate [translate x]
