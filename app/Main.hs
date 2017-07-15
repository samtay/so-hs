module Main where

import           Control.Monad.State.Strict (runStateT)

import           Cli                        (run)
import           StackOverflow              (google)

main :: IO ()
main = run >>= runStateT google >>= print
