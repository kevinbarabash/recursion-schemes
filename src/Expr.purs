module Expr where

import Control.Apply (lift2)
import Data.Array (zip)
import Data.Foldable (intercalate, sum, class Foldable, foldl)
import Data.Functor.Mu (Mu, roll, unroll)
import Data.Map (Map, lookup)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple, snd, uncurry)
import Effect (Effect)
import Effect.Exception (throw)
import Matryoshka.Fold (cata, para)
import Prelude (class Functor, class Semiring, map, one, pure, show, (#), ($), (*), (+), (-), (/), (<<<), (<>))


data ExprF a
  = Num Number
  | Var String
  | Add (Array a)
  | Sub a a
  | Mul (Array a)
  | Div a a

derive instance functorExprF :: Functor ExprF

type Expr = Mu ExprF

num :: Number -> Expr
num = roll <<< Num -- same as `num x = roll $ Num x`

var :: String -> Expr
var = roll <<< Var

add :: Array Expr -> Expr
add = roll <<< Add

sub :: Expr -> Expr -> Expr
sub x y = roll $ Sub x y

mul :: Array Expr -> Expr
mul = roll <<< Mul

div :: Expr -> Expr -> Expr
div x y = roll $ Div x y

showExpr :: Expr -> String
showExpr = cata showExprF -- cata as a generalized fold
  where
    showExprF :: ExprF String -> String
    showExprF (Num x) = show x -- toString()
    showExprF (Var x) = show x
    showExprF (Add args) = "(+ " <> intercalate " " args <> ")"
    showExprF (Sub x y) = "(- " <> x <> " " <> y <> ")"
    showExprF (Mul args) = "(* " <> intercalate " " args <> ")"
    showExprF (Div x y) = "(/ " <> x <> " " <> y <> ")"
    
prod :: ∀ a f. Foldable f => Semiring a => f a -> a
prod a = foldl (*) (one a) a

evalExpr :: Expr -> Number
evalExpr = cata evalExprF
  where
    evalExprF :: ExprF Number -> Number
    evalExprF (Num x) = x
    evalExprF (Var x) = 0.0
    evalExprF (Add args) = sum args
    evalExprF (Sub x y) = (-) x y -- same as `x - y`
    evalExprF (Mul args) = prod args
    evalExprF (Div x y) = (/) x y -- same as `x / y`

evalExpr' :: (Map String Number) -> Expr -> Maybe Number -- evalExpr'(map)(expr) => result
evalExpr' map = cata evalExprF
  where 
    evalExprF :: ExprF (Maybe Number) -> (Maybe Number)
    evalExprF (Num x) = pure x -- Maybe<Number>
    evalExprF (Var x) = lookup x map -- Maybe<Number> : Just Number | Nothing
    evalExprF (Add args) = foldl (lift2 (+)) (pure 0.0) args
    evalExprF (Sub x y) = lift2 (-) x y -- lifting, allows use of "normal" functions
    evalExprF (Mul args) = foldl (lift2 (*)) (pure 1.0) args
    evalExprF (Div x y) = lift2 (/) x y

evalExpr'' :: Expr -> Effect Number
evalExpr'' = cata evalExprF
  where 
    evalExprF :: ExprF (Effect Number) -> (Effect Number)
    evalExprF (Num x) = pure x -- Effect<Number>
    evalExprF (Var x) = throw $ show x <> " is not defined"
    evalExprF (Add args) = foldl (lift2 (+)) (pure 0.0) args
    evalExprF (Sub x y) = lift2 (-) x y
    evalExprF (Mul args) = foldl (lift2 (*)) (pure 1.0) args
    evalExprF (Div x y) = lift2 (/) x y

isAdd :: Expr -> Boolean
isAdd x = case unroll x of
  Add _ -> true
  _ -> false

showExpr' :: Expr -> String
showExpr' = para showExprF
  where
    showExprF :: ExprF (Tuple Expr String) -> String
    showExprF (Num x) = show x
    showExprF (Var x) = show x
    showExprF (Add args) = intercalate " + " $ map snd args
    showExprF (Sub x y) = (snd x) <> " - " <> (snd y)
    showExprF (Mul args) = intercalate " * " $ 
      map (uncurry (\x y -> if isAdd x then "(" <> y <> ")" else y)) args
    showExprF (Div x y) = (snd x) <> " / " <> (snd y)

type RAlgebra' f a = Mu f -> f a -> a

para'' :: forall f a. Functor f => RAlgebra' f a -> Mu f -> a
para'' alg t = unroll t # map (para'' alg) # alg t

showExpr'' :: Expr -> String
showExpr'' = para'' showExprF
  where
    showExprF :: RAlgebra' ExprF String -- Mu ExprF -> ExprF String -> String
    showExprF _ (Num x) = show x
    showExprF _ (Var x) = show x
    showExprF _ (Add args) = intercalate " + " args
    showExprF _ (Sub x y) = x <> " - " <> y
    showExprF _ (Div x y) = x <> " / " <> y
    showExprF expr (Mul args) = do
      case unroll expr of
        Mul args' -> do
          let pairs = zip args' args
          intercalate " * " $
            map (uncurry (\x y -> if isAdd x then "(" <> y <> ")" else y)) pairs
        _ -> "This should never happen"
