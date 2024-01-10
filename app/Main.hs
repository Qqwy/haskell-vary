{-# LANGUAGE DataKinds, TypeApplications #-}
{-# OPTIONS_GHC -ddump-stg-from-core #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE OverloadedLists, OverloadedStrings, MagicHash #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
module Main (main) where

import Data.Function ((&))
import Lib
import Vary (Vary)
import qualified Vary
import qualified Data.Vector.Unboxed as UVector
import Data.Array.Byte(ByteArray)
import GHC.Exts
import Control.Exception.Base
import Data.Word
import Foreign.C.String

main :: IO ()
main = do
    let str = example buildVar
    putStrLn str
    -- print foo 
    -- print world

buildVar :: Vary '[Bool, Int]
buildVar = Vary.from @Int 1234

example :: Vary (Bool : Int : l) -> String
-- example :: Vary [Bool, Int] -> String
example =
  Vary.defaultCase "hmm"
  -- Vary.case_
    & Vary.on @Int show
    & Vary.on boolFun
    & Vary.morphed

boolFun :: Bool -> String
boolFun x = if x then "true" else "false"

-- foo :: UVector.Vector Int
-- foo = UVector.fromList [1,2,3]

-- data Foo = Foo Addr#

-- instance Show Foo where
--   show (Foo val) = typeError val

hello :: Ptr Word8
hello = Ptr "hello"#

{-# NOINLINE world #-}
world :: (Int, Int, Int)
world = (1,2,3)
