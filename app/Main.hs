{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -ddump-stg-final #-}

module Main (main) where

import Control.Exception.Base
import Data.Array.Byte (ByteArray)
import Data.Function ((&))
-- import qualified Data.Vector.Unboxed as UVector
import Data.Word
import Foreign.C.String
import GHC.Exts
import Lib
import Vary (Vary, (:|))
import qualified Vary

main :: IO ()
main = do
  let vary = example3 buildVar :: Vary '[String, Bool]
  let (Just val) = Vary.into @String $ vary -- example2 buildVar -- example buildVar
  putStrLn (show val)

-- print foo
-- print world

buildVar :: (Bool :| l, Int :| l) => Vary l
-- buildVar :: Vary '[Bool, Int]
buildVar =
  if False
    then Vary.from @Bool True
    else Vary.from @Int 1234

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

hello :: Ptr Word8
hello = Ptr "hello"#

{-# NOINLINE world #-}
world :: (Int, Int, Int)
world = (1, 2, 3)

-- example2 :: Vary (Bool, Int] -> Vary '[String, String]
-- example2 :: Vary (Int : Bool : l) -> Vary (String : l)
-- example2 :: Vary xs -> Vary xs
-- example2 :: (Bool :| l, Int :| l) => Vary l -> Vary (String : String : l2)
-- example2 :: Vary xs -> Vary xs
-- example2 :: (Int :| l, String :| l2, l2 ~ Vary.Mapped Int String l) => Vary l -> Vary l2
-- example2 :: forall l. Vary l -> Vary l
-- example2 :: (Int :| l) => Vary l -> Vary l2
-- example2 :: (Vary.Mapped Int String (Int : xs) ys) => Vary (Int : xs) -> Vary ys
-- example2 :: (Vary.Mapped Int String xs (String : ys)) => Vary xs -> Vary (String : ys)
-- example2 = Vary.mapOn @Int show

example3 = Vary.mapOn3 @Int show

  -- let
  --   -- a :: Vary (String : Int : l)
  --   a = Vary.mapOn @Bool show vary
  --   -- b :: Vary (String : String : l)
  --   b = Vary.mapOn @Int show a
  --  in b
