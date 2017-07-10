module Main where

import Cli

main :: IO ()
main = run >>= print
