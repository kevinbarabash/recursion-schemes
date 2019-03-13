module Calc where
  
import Prelude

import Data.Array (head, tail)
import Data.String (Pattern(..))
import Data.String.Common (joinWith, split)
import Global (readFloat)
import Matryoshka.Coalgebra (Coalgebra)
import Matryoshka.Algebra (Algebra)
import Matryoshka.Refold (hylo)
import Data.Maybe (fromMaybe)
import Data.List ((:), List)

data Token
  = Lit Number
  | Op (Number -> Number -> Number)


parseToken :: String -> Token
parseToken "+" = Op (+)
parseToken "-" = Op (-)
parseToken "*" = Op (*)
parseToken "/" = Op div
parseToken num = Lit $ readFloat num

-- List a
--  = Cons a
--  | Nil

data InprogresWork a b
  = Cons a b
  | Nil

derive instance functorInprogresWork :: Functor (InprogresWork a)
    
parseRPN :: Coalgebra (InprogresWork Token) String
parseRPN ""  = Nil
parseRPN str = Cons token newSeed
  where
    chunks = split (Pattern " ") str
    token = parseToken $ fromMaybe "0" $ head chunks
    newSeed = joinWith " " $ fromMaybe [] (tail chunks)

type Stack = List Number

evalRPN :: Algebra (InprogresWork Token) (Stack -> Stack)
evalRPN Nil stack                      = stack
evalRPN (Cons (Lit i) cont) stack      = cont (i : stack)
evalRPN (Cons (Op fn) cont) (a:b:rest) = cont (fn b a : rest)
evalRPN _ stack                        = stack

rpn :: String -> Stack
rpn s = hylo evalRPN parseRPN s mempty

-- hylomorphism : generalized unfolding and folding
-- generalize folds -> generalized unfolding and folding
-- unfolding == parsing
-- 5 = 1 + 4 = 1 + 1 + 3 = ... = 1 + 1 + 1 + 1 + 1
