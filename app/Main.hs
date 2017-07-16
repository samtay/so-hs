module Main where

import           Control.Monad.State.Strict (runStateT)

import           Cli                        (run)
import           StackOverflow              (query)

main :: IO ()
main = run >>= runStateT query >>= print
