# vary

 == Motivating Example

 Say we are writing an image thumbnailing service.

 * Given an image URL
 * We attempt to download it.
     * This can fail, because the URL is incorrect;
     * Or the URL /is/ correct but the server could not be reached (in which case we want to re-try later);
     * Or the server /could/ be reached, but downloading took longer than a set time limit.

 * We pass it to a thumbnailing program.
     * This can fail, because the downloaded file might turn out actually not to be a valid image file (PNG or JPG);
     * Or even if the downloaded file /is/ an image, it might have a much too high resolution to attempt to read;


 The first instinct of many Haskell programmers is to write dedicated sum types for these errors like so:

```haskell
data Image = Image
  deriving (Eq, Show)

data DownloaderError1
    = IncorrectUrl1
    | ServerUnreachable1
    | DownloadingTimedOut1 
  deriving (Eq, Ord, Show)

data ThumbnailError1
    = NotAnImage1
    | TooBigImage1
  deriving (Eq, Ord, Show)

download1 :: String -> Either DownloaderError1 Image
download1 url = 
    -- Pretend we're doing hard work here
    Right Image

thumbnail1 :: Image -> Either ThumbnailError1 Image
thumbnail1 image = 
    -- Pretend we're huffing and puffing
    Right Image
```

But if we try to plainly combine these two functions, we get a compiler error:

```haskell
thumbnailService1 url = do
    image <- download1 url
    thumbnail <- thumbnail1 image
    pure thumbnail
```

```
error:
    • Couldn't match type ‘ThumbnailError’ with ‘DownloaderError’
      Expected: Either DownloaderError Image
        Actual: Either ThumbnailError Image
```


We could \'solve\' this problem by adding yet another manual error type:

```haskell
data ThumbnailServiceError1
    = DownloaderError1 DownloaderError1
    | ThumbnailError1 ThumbnailError1
      deriving (Eq, Ord, Show)

thumbnailService2 :: String -> Either ThumbnailServiceError1 Image
thumbnailService2 url = do
    image <- first DownloaderError1 $ download1 url
    thumb <- first ThumbnailError1 $ thumbnail1 image
    pure thumb
```


This 'works', although already we can see that we're doing a lot of manual ceremony to pass the errors around.

And wait! We wanted to re-try in the case of a `ServerUnreachable` error!

```haskell
waitAndRetry = undefined :: Word -> (() -> a)  -> a

thumbnailServiceRetry2 :: String -> Either ThumbnailServiceError1 Image
thumbnailServiceRetry2 url = 
  case download1 url of
    Left ServerUnreachable1 -> waitAndRetry 10 (\_ -> thumbnailServiceRetry2 url) 
    Left other -> Left (DownloaderError1 other)
    Right image -> do
      thumb <- first ThumbnailError1 $ thumbnail1 image
      pure thumb
```

We now see:

- Even inside `thumbnailService` there now is quite a bit of ceremony 
  w.r.t. wrapping,unwrapping and mapping between error types.
- Callers will be asked to pattern match on the @ServerUnreachable@ error case,
  even though that case will already be handled inside the `thumbnailService` function itself!
- Imagine what happens when using this small function in a bigger system with many more errors!
  Do you keep defining more and more wrapper types for various combinations of errors?

#### There is a better way!

With the `Vary` and related `Vary.VEither.VEither` types, you can mix and match individual errors (or other types) at the places they are used.

- No more wrapper type definitions!
- Handing an error makes it go away from the outcome type!

```haskell top:1
import Vary (Vary, (:|))
import qualified Vary
import Vary.VEither (VEither(..))
import qualified Vary.VEither as VEither
```
```haskell
data IncorrectUrl2 = IncorrectUrl2 deriving (Eq, Ord, Show)
data ServerUnreachable2 = ServerUnreachable2 deriving (Eq, Ord, Show)
data DownloadingTimedOut2 = DownloadingTimedOut2 deriving (Eq, Ord, Show)

data NotAnImage2 = NotAnImage2 deriving (Eq, Ord, Show)
data TooBigImage2 = TooBigImage2 deriving (Eq, Ord, Show)

download :: (ServerUnreachable2 :| err, IncorrectUrl2 :| err) => String -> VEither err Image
download url = 
    -- Pretend a lot of network communication happens here
    VRight Image

thumbnail :: (NotAnImage2 :| err, TooBigImage2 :| err) => Image -> VEither err Image
thumbnail image = 
    -- Pretend this is super hard
    VRight Image
```

Here is the version without the retry:

```haskell
thumbnailService :: String -> VEither [ServerUnreachable2, IncorrectUrl2, NotAnImage2, TooBigImage2] Image
thumbnailService url = do
  image <- download url
  thumb <- thumbnail image
  pure thumb
```

And here is all that needed to change to have a retry:

```haskell
thumbnailServiceRetry :: String -> VEither [IncorrectUrl2, NotAnImage2, TooBigImage2] Image
thumbnailServiceRetry url = do
  image <- download url 
           & VEither.onLeft (\ServerUnreachable2 -> waitAndRetry 10 (\_ -> thumbnailServiceRetry url)) id
  thumb <- thumbnail image
  pure thumb
```

<!-- 
The following is executed by the README test runner,
but we don't want it to be visible to human readers:

```haskell top:0
{-# OPTIONS_GHC -fdefer-type-errors #-} -- We want to show some incorrect examples!
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DataKinds #-}
module Main where

import Test.Hspec (hspec, describe, it, shouldBe)
import Test.ShouldNotTypecheck (shouldNotTypecheck)
import Data.Bifunctor (first)
import Data.Function ((&))

```

```haskell
main :: IO ()
main = hspec $ do
  describe "bad v1" $ do
    it "should not typecheck" $
      shouldNotTypecheck (thumbnailService1)
  describe "bad v2" $ do
    it "should work (but be verbose)" $
      thumbnailService2 "http://example.com" `shouldBe` (Right Image)
  describe "bad v2 (wth retry)" $ do
    it "should work (but be super verbose)" $
      thumbnailServiceRetry2 "http://example.com" `shouldBe` (Right Image)
  describe "nice" $ do
    it "should work nicely" $
      thumbnailService "http://example.com" `shouldBe` (VRight Image)
  describe "nice (with retry)" $ do
    it "should work nicely" $
      thumbnailServiceRetry "http://example.com" `shouldBe` (VRight Image)
```
-->
