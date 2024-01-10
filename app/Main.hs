{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -ddump-stg-from-core #-}

module Main (main) where

import Control.Exception.Base
import Data.Array.Byte (ByteArray)
import Data.Function ((&))
import qualified Data.Vector.Unboxed as UVector
import Data.Word
import Foreign.C.String
import GHC.Exts
import Lib
import Vary (Vary)
import qualified Vary

main :: IO ()
main = do
  let str = example buildVar
  putStrLn str

-- print foo
-- print world

buildVar :: Vary '[Bool, Int]
buildVar = Vary.from @Int 1234

-- example :: Vary (Bool : Int : l) -> String
example :: Vary [Bool, Int] -> String
example =
      Vary.morphed
      $ Vary.on @Int show
      $ Vary.on boolFun
      $ Vary.exhaustiveCase
      -- $ Vary.defaultCase "Hmm"
 --Vary.defaultCase "hmm"
-- Vary.exhaustiveCase
-- & Vary.on boolFun
-- & Vary.on @Int show
-- & Vary.morphed

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
world = (1, 2, 3)
