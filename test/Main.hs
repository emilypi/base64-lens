{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}

module Main (main) where

import Control.Lens
import Data.String (IsString)
import Data.Word (Word8)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Short as SBS
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Short as TS
import qualified Data.ByteString.Base64.Lens as B
import qualified Data.ByteString.Lazy.Base64.Lens as L
import qualified Data.ByteString.Short.Base64.Lens as S
import qualified Data.Text.Encoding.Base64.Lens as U
import qualified Data.Text.Lazy.Encoding.Base64.Lens as V
import qualified Data.Text.Short.Encoding.Base64.Lens as W
import Data.Text.Encoding.Base64.Error (Base64Error(..))
import Data.Text.Encoding.Base64.Error.Lens
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

-- Exercise the same public contract for each representation, including patterns.
data Optics a = Optics
    { standard :: Prism' a a
    , url :: Prism' a a
    , unpadded :: Prism' a a
    , lenient :: Iso' a a
    , urlLenient :: Iso' a a
    , patterns :: [(String, a -> a, a -> Maybe a)]
    }

main :: IO ()
main = defaultMain $ testGroup "base64-lens"
    [ suite "ByteString" byteStringOptics
    , suite "Lazy ByteString" lazyByteStringOptics
    , suite "ShortByteString" shortByteStringOptics
    , suite "Text" textOptics
    , suite "Lazy Text" lazyTextOptics
    , suite "ShortText" shortTextOptics
    , roundtrips "ByteString binary" byteStringOptics (BS.pack :: [Word8] -> BS.ByteString)
    , roundtrips "Lazy ByteString binary, one byte per chunk" lazyByteStringOptics
        (BL.fromChunks . map BS.singleton :: [Word8] -> BL.ByteString)
    , roundtrips "ShortByteString binary" shortByteStringOptics (SBS.pack :: [Word8] -> SBS.ShortByteString)
    , roundtrips "Text Unicode" textOptics T.pack
    , roundtrips "Lazy Text Unicode, one character per chunk" lazyTextOptics (TL.fromChunks . map T.singleton)
    , roundtrips "ShortText Unicode" shortTextOptics TS.pack
    , testGroup "Error prisms"
        [ testCase "construct decode error" $
            (_DecodeError # "invalid" :: Base64Error Int) @?= DecodeError "invalid"
        , testCase "construct conversion error" $
            (_ConversionError # 42 :: Base64Error Int) @?= ConversionError 42
        , testCase "select decode error" $
            preview _DecodeError (DecodeError "invalid" :: Base64Error Int) @?= Just "invalid"
        , testCase "select conversion error" $
            preview _ConversionError (ConversionError 42 :: Base64Error Int) @?= Just 42
        , testCase "reject conversion error as decode error" $
            preview _DecodeError (ConversionError 42 :: Base64Error Int) @?= Nothing
        , testCase "reject decode error as conversion error" $
            preview _ConversionError (DecodeError "invalid" :: Base64Error Int) @?= Nothing
        ]
    ]

suite :: (Eq a, Show a, IsString a) => String -> Optics a -> TestTree
suite name o = testGroup name
    [ testGroup "RFC 4648 vectors"
        [ testCase (show raw) $ do
            review (standard o) raw @?= encoded
            preview (standard o) encoded @?= Just raw
            review (url o) raw @?= encoded
            preview (url o) encoded @?= Just raw
            preview (url o) bare @?= Just raw
            review (unpadded o) raw @?= bare
            preview (unpadded o) bare @?= Just raw
        | (raw, encoded, bare) <-
            [ ("", "", ""), ("f", "Zg==", "Zg"), ("fo", "Zm8=", "Zm8")
            , ("foo", "Zm9v", "Zm9v"), ("foob", "Zm9vYg==", "Zm9vYg")
            , ("fooba", "Zm9vYmE=", "Zm9vYmE"), ("foobar", "Zm9vYmFy", "Zm9vYmFy")
            ]
        ]
    , testCase "distinct alphabets" $ do
        review (standard o) "<<???>>" @?= "PDw/Pz8+Pg=="
        review (url o) "<<???>>" @?= "PDw_Pz8-Pg=="
        preview (standard o) "PDw_Pz8-Pg==" @?= Nothing
        preview (url o) "PDw/Pz8+Pg==" @?= Nothing
    , testGroup "reject malformed standard encoding"
        [ testCase (show bad) $ preview (standard o) bad @?= Nothing
        | bad <- ["%", "Z", "Zg", "Zg=", "Zg===", "=m9v", "Zm=v", "Zh==", "Zm9=", "Zg==AAAA", "Zm9v\n"]
        ]
    , testGroup "reject malformed URL encoding"
        [ testCase (show bad) $ preview (url o) bad @?= Nothing
        | bad <- ["%", "Z", "Zg=", "Zh==", "Zh", "Zm9", "Zm9=", "Zg==AAAA"]
        ]
    , testCase "unpadded rejects padding and noncanonical bits" $ do
        preview (unpadded o) "Zg==" @?= Nothing
        preview (unpadded o) "Zm8=" @?= Nothing
        preview (unpadded o) "Zh" @?= Nothing
        preview (unpadded o) "Zm9" @?= Nothing
    , testCase "lenient optics encode and decode in the expected directions" $ do
        review (lenient o) "Sun" @?= "U3Vu"
        view (lenient o) "U3!Vu" @?= "Sun"
        review (urlLenient o) "<<???>>" @?= "PDw_Pz8-Pg=="
        view (urlLenient o) "PDw_!Pz8-Pg==" @?= "<<???>>"
    , testGroup "bidirectional patterns"
        [ testCase patternName $ do
            build "Sun" @?= "U3Vu"
            match "U3Vu" @?= Just "Sun"
            if patternName `elem` ["Base64Lenient", "Base64UrlLenient"]
                then match "U3!Vu" @?= Just "Sun"
                else match "U3!Vu" @?= Nothing
        | (patternName, build, match) <- patterns o
        ]
    ]

roundtrips :: (Arbitrary b, Show b, Eq a, Show a) => String -> Optics a -> (b -> a) -> TestTree
roundtrips name o pack = testGroup name
    [ testProperty "standard prism" $ \input ->
        let raw = pack input in preview (standard o) (review (standard o) raw) === Just raw
    , testProperty "URL prism" $ \input ->
        let raw = pack input in preview (url o) (review (url o) raw) === Just raw
    , testProperty "unpadded URL prism" $ \input ->
        let raw = pack input in preview (unpadded o) (review (unpadded o) raw) === Just raw
    , testProperty "lenient optic" $ \input ->
        let raw = pack input in view (lenient o) (review (lenient o) raw) === raw
    , testProperty "lenient URL optic" $ \input ->
        let raw = pack input in view (urlLenient o) (review (urlLenient o) raw) === raw
    , testGroup "pattern roundtrips"
        [ testProperty patternName $ \input ->
            let raw = pack input in match (build raw) === Just raw
        | (patternName, build, match) <- patterns o
        ]
    ]

byteStringOptics :: Optics BS.ByteString
byteStringOptics = Optics B._Base64 B._Base64Url B._Base64UrlUnpadded B._Base64Lenient B._Base64UrlLenient
    [ ("Base64", B.Base64, \case B.Base64 a -> Just a; _ -> Nothing)
    , ("Base64Url", B.Base64Url, \case B.Base64Url a -> Just a; _ -> Nothing)
    , ("Base64UrlUnpadded", B.Base64UrlUnpadded, \case B.Base64UrlUnpadded a -> Just a; _ -> Nothing)
    , ("Base64Lenient", B.Base64Lenient, \case B.Base64Lenient a -> Just a)
    , ("Base64UrlLenient", B.Base64UrlLenient, \case B.Base64UrlLenient a -> Just a)
    ]

lazyByteStringOptics :: Optics BL.ByteString
lazyByteStringOptics = Optics L._Base64 L._Base64Url L._Base64UrlUnpadded L._Base64Lenient L._Base64UrlLenient
    [ ("Base64", L.Base64, \case L.Base64 a -> Just a; _ -> Nothing)
    , ("Base64Url", L.Base64Url, \case L.Base64Url a -> Just a; _ -> Nothing)
    , ("Base64UrlUnpadded", L.Base64UrlUnpadded, \case L.Base64UrlUnpadded a -> Just a; _ -> Nothing)
    , ("Base64Lenient", L.Base64Lenient, \case L.Base64Lenient a -> Just a)
    , ("Base64UrlLenient", L.Base64UrlLenient, \case L.Base64UrlLenient a -> Just a)
    ]

shortByteStringOptics :: Optics SBS.ShortByteString
shortByteStringOptics = Optics S._Base64 S._Base64Url S._Base64UrlUnpadded S._Base64Lenient S._Base64UrlLenient
    [ ("Base64", S.Base64, \case S.Base64 a -> Just a; _ -> Nothing)
    , ("Base64Url", S.Base64Url, \case S.Base64Url a -> Just a; _ -> Nothing)
    , ("Base64UrlUnpadded", S.Base64UrlUnpadded, \case S.Base64UrlUnpadded a -> Just a; _ -> Nothing)
    , ("Base64Lenient", S.Base64Lenient, \case S.Base64Lenient a -> Just a)
    , ("Base64UrlLenient", S.Base64UrlLenient, \case S.Base64UrlLenient a -> Just a)
    ]

textOptics :: Optics T.Text
textOptics = Optics U._Base64 U._Base64Url U._Base64UrlUnpadded U._Base64Lenient U._Base64UrlLenient
    [ ("Base64", U.Base64, \case U.Base64 a -> Just a; _ -> Nothing)
    , ("Base64Url", U.Base64Url, \case U.Base64Url a -> Just a; _ -> Nothing)
    , ("Base64UrlUnpadded", U.Base64UrlUnpadded, \case U.Base64UrlUnpadded a -> Just a; _ -> Nothing)
    , ("Base64Lenient", U.Base64Lenient, \case U.Base64Lenient a -> Just a)
    , ("Base64UrlLenient", U.Base64UrlLenient, \case U.Base64UrlLenient a -> Just a)
    ]

lazyTextOptics :: Optics TL.Text
lazyTextOptics = Optics V._Base64 V._Base64Url V._Base64UrlUnpadded V._Base64Lenient V._Base64UrlLenient
    [ ("Base64", V.Base64, \case V.Base64 a -> Just a; _ -> Nothing)
    , ("Base64Url", V.Base64Url, \case V.Base64Url a -> Just a; _ -> Nothing)
    , ("Base64UrlUnpadded", V.Base64UrlUnpadded, \case V.Base64UrlUnpadded a -> Just a; _ -> Nothing)
    , ("Base64Lenient", V.Base64Lenient, \case V.Base64Lenient a -> Just a)
    , ("Base64UrlLenient", V.Base64UrlLenient, \case V.Base64UrlLenient a -> Just a)
    ]

shortTextOptics :: Optics TS.ShortText
shortTextOptics = Optics W._Base64 W._Base64Url W._Base64UrlUnpadded W._Base64Lenient W._Base64UrlLenient
    [ ("Base64", W.Base64, \case W.Base64 a -> Just a; _ -> Nothing)
    , ("Base64Url", W.Base64Url, \case W.Base64Url a -> Just a; _ -> Nothing)
    , ("Base64UrlUnpadded", W.Base64UrlUnpadded, \case W.Base64UrlUnpadded a -> Just a; _ -> Nothing)
    , ("Base64Lenient", W.Base64Lenient, \case W.Base64Lenient a -> Just a)
    , ("Base64UrlLenient", W.Base64UrlLenient, \case W.Base64UrlLenient a -> Just a)
    ]
