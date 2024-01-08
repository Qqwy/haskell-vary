{-# LANGUAGE DataKinds, TypeApplications #-}
{-# OPTIONS_GHC -ddump-stg-final #-}
module Main (main) where

import Data.Function ((&))
import Lib
import Vary (Vary)
import qualified Vary

main :: IO ()
main = do
    let str = example buildVar
    putStrLn str

buildVar :: Vary '[Bool, Int]
buildVar = Vary.from @Int 10

example :: Vary '[Bool, Int] -> String
example =
  -- defaultCase "hmm"
  Vary.case_
    & Vary.on @Int show
    & Vary.on boolFun
    & Vary.morphed

boolFun :: Bool -> String
boolFun x = if x then "true" else "false"