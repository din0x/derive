module Solver where

import Ast (Expr (Infix, Number, Symbol), Op (Add, Mul))
import Data.Function (on)
import Data.List (groupBy, sortOn)

fix :: (Eq a) => (a -> a) -> a -> a
fix f x =
    let x' = f x
     in if x == x' then x else fix f x'

expand :: Expr -> Expr
expand = fix expand1
  where
    expand1 (Number i) = Number i
    expand1 (Symbol s) = Symbol s
    expand1 (Infix op (Number l) (Number r)) = Number (perform op l r)
    expand1 (Infix op0 (Number l0) (Infix op1 (Number l1) r1))
        | op0 == op1 = Infix op0 (Number (perform op0 l0 l1)) r1
    expand1 (Infix op l r) =
        let terms = map (\e -> (1, e)) (collect op (Infix op l r))
         in let grouped = group terms
             in join op (map rebuild grouped)

join :: Op -> [Expr] -> Expr
join op [] = identity op
join _ [x] = x
join op (x : xs) = Infix op x (join op xs)

rebuild :: (Expr, Int) -> Expr
rebuild (Number 1, c) = Number c
rebuild (_, 0) = Number 0
rebuild (base, 1) = base
rebuild (base, c) = Infix Mul (Number c) base

perform :: Op -> Int -> Int -> Int
perform Add a b = a + b
perform Mul a b = a * b

identity :: Op -> Expr
identity Add = Number 0
identity Mul = Number 1

group :: (Ord expr) => [(Int, expr)] -> [(expr, Int)]
group xs =
    map combine grouped
  where
    grouped =
        groupBy ((==) `on` snd)
            . sortOn snd
            $ xs

    combine grp =
        let base = snd (head grp)
            coeff = sum (map fst grp)
         in (base, coeff)

collect :: Op -> Expr -> [Expr]
collect op (Infix op' l r)
    | op == op' = collect op l ++ collect op r
collect _ e = [e]
