module Main where

import Ast (parser)
import Parser
import Solver (expand)

main :: IO ()
main = do
    line <- getLine
    let expr = fmap snd (p line)
    let result = fmap expand expr
    print result
    main
  where
    Parser p = parser
