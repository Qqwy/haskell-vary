{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DataKinds #-}
module Main (main) where

import Control.Exception.Base
import Data.Array.Byte (ByteArray)
import Data.Function ((&))
-- import qualified Data.Vector.Unboxed as UVector
import Data.Word
import Foreign.C.String
import GHC.Exts
import Vary (Vary, (:|))
import qualified Vary
import qualified Vary.Utils
import Data.Char (ord)

main :: IO ()
main = do
  let str = Vary.into @String $ example2 buildVar
  putStrLn (show str)

-- print foo
-- print world

-- buildVar :: Vary '[Bool, Int]
buildVar :: (Int :| l) => Vary l
buildVar = Vary.from @Int 1234

-- example :: Vary (Bool : Int : l) -> String
example :: Vary [Bool, Int] -> String
example =
  Vary.morphed $
    Vary.on @Int show $
      Vary.on boolFun $
        Vary.exhaustiveCase

-- \$ Vary.defaultCase "Hmm"
-- Vary.defaultCase "hmm"
--  Vary.exhaustiveCase
--  & Vary.on boolFun
--  & Vary.on @Int show
--  & Vary.morphed

boolFun :: Bool -> String
boolFun x = if x then "true" else "false"

-- foo :: UVector.Vector Int
-- foo = UVector.fromList [1,2,3]

-- data Foo = Foo Addr#

-- instance Show Foo where
--   show (Foo val) = typeError val

-- hello :: Ptr Word8
-- hello = Ptr "hello"#

{-# NOINLINE world #-}
world :: (Int, Int, Int)
world = (1, 2, 3)

-- example2 :: (Int :| xs, String :| ys, Vary.Utils.Mappable Int String xs ys) => Vary xs -> Vary ys
-- example2 :: Vary (Bool : Int : l) -> Vary (Bool : String : l)
-- example2 :: Vary.Utils.Mappable Int String xs ys => Vary xs -> Vary ys
example2 :: Vary '[Int, Bool] -> Vary '[String, Bool]
example2 vary = Vary.mapOn @Int show $ vary

-- example3 :: Vary (Bool : String : Int : l) -> Vary (String : l)
example3 vary = 
  vary
  & Vary.mapOn @Int show
  & Vary.mapOn @String show
  & Vary.mapOn @Bool show
  & Vary.morph

-- example4 :: (Vary.Utils.Mappable Int Bool xs ys, Vary.Utils.Mappable Char Int ys zs) => Vary xs -> Vary zs
example4 vary =
  vary
  & Vary.mapOn @Char ord
  & Vary.mapOn @Int (\x -> x > 0)

-- hmm :: (Show (Vary )) => Vary (a : l) -> String
hmm vary = show @(Vary _) vary
