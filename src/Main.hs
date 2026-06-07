module Main where

import Parser
import Ast (parser)

main :: IO ()
main = do
    let Parser p = parser
    line <- getLine
    print (p line)
