{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Main where

import Test.DocTest (mainFromCabal)
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    let args' = ["--no-implicit-module-import"] <> args
    mainFromCabal "vary" args'
