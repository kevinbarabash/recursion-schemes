module Ignore where
-- TODO: figure out why this ionly removes + 0.0 at the top level
-- removeAddZero :: Expr -> Maybe Expr
-- removeAddZero = para removeAddZeroF
--   where
--     removeAddZeroF :: ExprF (Tuple Expr (Maybe Expr)) -> Maybe Expr
--     removeAddZeroF (Add args) = do
--       let 
--         args' = (foldl (\acc x -> 
--           case snd x of
--             Just y -> case unroll y of
--               Num 0.0 -> acc
--               _ -> acc <> [y]
--             Nothing -> acc
--           ) [] args)
--       case args' of
--         [] -> Nothing
--         [x] -> Just x
--         xs -> Just (roll $ Add xs)
--     removeAddZeroF fx = roll fx

-- removeAddZero' :: Expr -> Maybe Expr
-- removeAddZero' = para'' removeAddZeroF'
--   where
--     removeAddZeroF' :: RAlgebra' ExprF (Maybe Expr) -- Mu ExprF -> ExprF (Maybe Expr) -> Maybe Expr
--     removeAddZeroF' expr (Add args) = do
--       let 
--         args' = (foldl (\acc x -> 
--           case x of
--             Just y -> case unroll y of
--               Num 0.0 -> acc
--               _ -> acc <> [y]
--             Nothing -> acc
--           ) [] args)
--       case args' of
--         [] -> Nothing
--         [x] -> Just x
--         xs -> Just (roll $ Add xs)
--     removeAddZeroF' x _ = Just x

