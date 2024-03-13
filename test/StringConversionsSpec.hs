{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module StringConversionsSpec where

------------------------------------------------------------------------------
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Lazy      as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSLU
import qualified Data.ByteString.UTF8      as BSU
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as TE
import qualified Data.Text.Lazy            as TL
import qualified Data.Text.Lazy.Encoding   as TLE
------------------------------------------------------------------------------
import qualified Prelude
import           Relude                    as R
import           Test.Hspec
------------------------------------------------------------------------------
{-# HLINT ignore "Use alternative" #-}
{-# HLINT ignore "Use decodeUtf8"  #-}
{-# HLINT ignore "Use encodeUtf8"  #-}
{-# HLINT ignore "Use toString"    #-}
{-# HLINT ignore "Use toLText"     #-}
{-# HLINT ignore "Use toText"      #-}
------------------------------------------------------------------------------

-- When using Relude, use the conversions marked with   -- <-- **

spec :: Spec
spec  = describe "StringConversions" $ do
  stringToFromByteString
  stringToFromText
  byteStringToFromText
  lazyToFromStrict

------------------------------------------------------------------------------
stringToFromByteString :: Spec
stringToFromByteString  = describe "stringToFromByteString" $ do

  --  BS.ByteString  -> String
  it "BS.ByteString  -> String         explicit"      $ do
    BSU.toString bsByteString    `shouldBe` ("bsByteString" :: Prelude.String)
  it "BS.ByteString  -> String         RELUDE"        $ do
    R.decodeUtf8 bsByteString    `shouldBe` ("bsByteString" :: Prelude.String)  -- <-- **

  --  String         -> BS.ByteString
  it "String         -> BS.ByteString  explicit"      $ do
    BSU.fromString string        `shouldBe` ("string" :: BS.ByteString)
  it "String         -> BS.ByteString  RELUDE"        $ do
    R.encodeUtf8   string        `shouldBe` ("string" :: BS.ByteString)         -- <-- **

  --  BSL.ByteString -> String
  it "BSL.ByteString -> String         explicit"      $ do
    BSLU.toString bslByteString  `shouldBe` ("bslByteString" :: Prelude.String)
  it "BSL.ByteString -> String         RELUDE"        $ do
    R.decodeUtf8  bslByteString  `shouldBe` ("bslByteString" :: Prelude.String) -- <-- **

  --  String         -> BSL.ByteString
  it "String         -> BSL.ByteString explicit"      $ do
    BSLU.fromString string       `shouldBe` ("string" :: BSL.ByteString)
  it "String         -> BSL.ByteString RELUDE"        $ do
    R.encodeUtf8    string       `shouldBe` ("string" :: BSL.ByteString)        -- <-- **


------------------------------------------------------------------------------
stringToFromText :: Spec
stringToFromText  = describe "stringToFromText" $ do

  --  T.Text         -> String
  it "T.Text         -> String         explicit"      $ do
    T.unpack   tText             `shouldBe` ("tText" :: Prelude.String)
  it "T.Text         -> String         RELUDE"        $ do
    R.toString tText             `shouldBe` ("tText" :: Prelude.String)         -- <-- **

  --  String         -> T.Text
  it "String         -> T.Text         explicit"      $ do
    T.pack   string              `shouldBe` ("string" :: T.Text)
  it "String         -> T.Text         RELUDE"        $ do
    R.toText string              `shouldBe` ("string" :: T.Text)                -- <-- **

  --  TL.Text        -> String
  it "TL.Text        -> String         explicit"      $ do
    TL.unpack  tlText            `shouldBe` ("tlText" :: Prelude.String)
  it "TL.Text        -> String         RELUDE"        $ do
    R.toString tlText            `shouldBe` ("tlText" :: Prelude.String)        -- <-- **

  --  String         -> TL.Text
  it "String         -> TL.Text        explicit"      $ do
    TL.pack string               `shouldBe` ("string" :: TL.Text)
  it "String         -> TL.Text        RELUDE"        $ do
    toLText string               `shouldBe` ("string" :: TL.Text)               -- <-- **

------------------------------------------------------------------------------
byteStringToFromText :: Spec
byteStringToFromText  = describe "byteStringToFromText" $ do

  --  TL.Text        -> BSL.ByteString
  it "TL.Text        -> BSL.ByteString explicit"      $ do
    TLE.encodeUtf8 tlText        `shouldBe` ("tlText" :: BSL.ByteString)
  it "TL.Text        -> BSL.ByteString RELUDE"        $ do
    R.encodeUtf8   tlText        `shouldBe` ("tlText" :: BSL.ByteString)        -- <-- **

  --  BSL.ByteString -> TL.Text
  it "BSL.ByteString -> TL.Text        explicit"      $ do
    TLE.decodeUtf8 bslByteString `shouldBe` ("bslByteString" :: TL.Text)
  it "BSL.ByteString -> TL.Text        RELUDE"        $ do
    R.decodeUtf8   bslByteString `shouldBe` ("bslByteString" :: TL.Text)        -- <-- **

  --  T.Text         -> BS.ByteString
  it "T.Text         -> BS.ByteString  explicit"      $ do
    TE.encodeUtf8 tText          `shouldBe` ("tText" :: BS.ByteString)
  it "T.Text         -> BS.ByteString  RELUDE"        $ do
    R.encodeUtf8  tText          `shouldBe` ("tText" :: BS.ByteString)          -- <-- **

  --  BS.ByteString  -> T.Text
  it "BS.ByteString  -> T.Text         explicit"      $ do
    TE.decodeUtf8 bsByteString   `shouldBe` ("bsByteString" :: T.Text)
  it "BS.ByteString  -> T.Text         RELUDE"        $ do
    R.decodeUtf8  bsByteString   `shouldBe` ("bsByteString" :: T.Text)          -- <-- **

------------------------------------------------------------------------------
lazyToFromStrict :: Spec
lazyToFromStrict  = describe "lazyToFromStrict" $ do

  --  BS.ByteString  -> BSL.ByteString
  it "BS.ByteString  -> BSL.ByteString explicit"      $ do
    BSL.fromStrict bsByteString  `shouldBe` ("bsByteString" :: BSL.ByteString)
  it "BS.ByteString  -> BSL.ByteString RELUDE"        $ do
    R.fromStrict   bsByteString  `shouldBe` ("bsByteString" :: BSL.ByteString)  -- <-- **

  --  BSL.ByteString -> BS.ByteString
  it "BSL.ByteString -> BS.ByteString  explicit"      $ do
    BSL.toStrict bslByteString   `shouldBe` ("bslByteString" :: BS.ByteString)
  it "BSL.ByteString -> BS.ByteString  RELUDE"        $ do
    R.toStrict   bslByteString   `shouldBe` ("bslByteString" :: BS.ByteString)  -- <-- **

  --  T.Text         -> TL.Text
  it "T.Text         -> TL.Text        explicit"      $ do
    TL.fromStrict tText          `shouldBe` ("tText" :: TL.Text)
  it "T.Text         -> TL.Text        RELUDE"        $ do
    R.fromStrict  tText          `shouldBe` ("tText" :: TL.Text)                -- <-- **

  --  TL.Text        -> T.Text
  it "TL.Text        -> T.Text         explicit"      $ do
    TL.toStrict tlText           `shouldBe` ("tlText" :: T.Text)
  it "TL.Text        -> T.Text         RELUDE"        $ do
    R.toStrict  tlText           `shouldBe` ("tlText" :: T.Text)                -- <-- **

------------------------------------------------------------------------------

string        :: Prelude.String
string         = "string"

bsByteString  :: BS.ByteString
bsByteString   = "bsByteString"

bslByteString :: BSL.ByteString
bslByteString  = "bslByteString"

tText         :: T.Text
tText          = "tText"

tlText        :: TL.Text
tlText         = "tlText"


