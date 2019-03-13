module Main where

import Prelude

import Data.Tuple (Tuple(..))
import Calc (rpn)
import Data.Map (empty, fromFoldable)
import Effect (Effect)
import Effect.Console (log)
import Expr (add, evalExpr, evalExpr', mul, num, showExpr, showExpr', showExpr'', evalExpr'', var)

main :: Effect Unit
main = do
  let expr1 = mul [(num 4.0), (add [(num 1.0), (num 2.0), (num 3.0)])]
  log $ showExpr expr1

  log $ show $ evalExpr expr1
  
  log $ showExpr' expr1
  log $ showExpr'' expr1
  
  let expr2 = mul [(var "a"), (var "b"), (var "c")] -- a * b * c
  log $ show $ evalExpr' empty expr2
  
  -- build a map of values from String to Number
  let vars = fromFoldable [(Tuple "a" 1.0), (Tuple "b" 2.0), (Tuple "c" 3.0)]

  -- evaluate expression using that map
  log $ show $ evalExpr' vars expr2
  -- log(show(evalExpr'(vars, expr2)))
  -- (log (show (evalExpr' vars expr2)))
  -- log $ showExpr expr2 <> " = " <> (show $ evalExpr' vars expr2)

  -- example using Effect as a wrapper since we it throw, uncomment to see the exception
  -- let expr3 = add [num 1.0, num 0.0, mul[var "a", add[num 2.0, num 0.0] ] ]
  -- _ <- evalExpr'' expr3

  let res = rpn "1 2 +"
  log $ "1 2 + = " <> show res

  log "end of line" -- cute
