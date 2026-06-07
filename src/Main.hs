module Main where

import Ast (parser)
import Parser

main :: IO ()
main = do
    let Parser p = parser
    line <- getLine
    print (p line)
    main
