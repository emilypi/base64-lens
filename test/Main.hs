{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}

module Main (main) where

import Control.Lens
import Data.Base64.Types (Alphabet(..), Base64, assertBase64, extractBase64)
import qualified Data.Base64.Types.Lens as Wrapped
import Data.String (IsString)
import Data.Word (Word8)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Short as SBS
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Short as TS
import qualified Data.ByteString.Base64 as Base64
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

-- Check both directions of each optic and its corresponding typed pattern.
data Codec k a = Codec
    { optic :: Iso' (Base64 k a) a
    , construct :: a -> Base64 k a
    , deconstruct :: Base64 k a -> a
    }

data Optics a = Optics
    { standard :: Codec 'StdPadded a
    , url :: Codec 'UrlPadded a
    , unpadded :: Codec 'UrlUnpadded a
    , lenient :: Codec 'NonStandard a
    , urlLenient :: Codec 'NonStandard a
    , standardUntyped :: Prism' a a
    , urlUntyped :: Prism' a a
    , unpaddedUntyped :: Prism' a a
    }

vectors :: IsString a => [(a, a, a)]
vectors =
    [ ("", "", ""), ("f", "Zg==", "Zg"), ("fo", "Zm8=", "Zm8")
    , ("foo", "Zm9v", "Zm9v"), ("foob", "Zm9vYg==", "Zm9vYg")
    , ("fooba", "Zm9vYmE=", "Zm9vYmE"), ("foobar", "Zm9vYmFy", "Zm9vYmFy")
    ]

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
    , testGroup "Untyped prisms"
        [ untypedSuite "ByteString" byteStringOptics
        , untypedSuite "Lazy ByteString" lazyByteStringOptics
        , untypedSuite "ShortByteString" shortByteStringOptics
        , untypedSuite "Text" textOptics
        , untypedSuite "Lazy Text" lazyTextOptics
        , untypedSuite "ShortText" shortTextOptics
        ]
    , chunkedPrismTests
    , wrapperTests
    , alphabetTests
    , errorTests
    , testCase "parse untrusted input before typed decoding" $ do
        fmap (view B._Base64) (Base64.parseBase64 "U3Vu") @?= Right "Sun"
        case Base64.parseBase64 "U3V=" of
            Left _ -> pure ()
            Right _ -> assertFailure "Non-canonical input was accepted"
    ]

suite :: (Eq a, Show a, IsString a) => String -> Optics a -> TestTree
suite name o = testGroup name
    [ testGroup "RFC 4648 vectors"
        [ testCase (show raw) $ do
            check (standard o) raw encoded
            check (url o) raw encoded
            check (unpadded o) raw bare
            check (lenient o) raw encoded
            check (urlLenient o) raw encoded
        | (raw, encoded, bare) <- vectors
        ]
    , testCase "distinct alphabets and padding" $ do
        check (standard o) "<<???>>" "PDw/Pz8+Pg=="
        check (url o) "<<???>>" "PDw_Pz8-Pg=="
        check (unpadded o) "<<???>>" "PDw_Pz8-Pg"
        check (lenient o) "<<???>>" "PDw/Pz8+Pg=="
        check (urlLenient o) "<<???>>" "PDw_Pz8-Pg=="
    , testCase "lenient decoding of non-standard wrappers" $ do
        view (optic (lenient o)) (assertBase64 "U3!Vu") @?= "Sun"
        deconstruct (lenient o) (assertBase64 "U3!Vu") @?= "Sun"
        view (optic (urlLenient o)) (assertBase64 "PDw_!Pz8-Pg==") @?= "<<???>>"
        deconstruct (urlLenient o) (assertBase64 "PDw_!Pz8-Pg==") @?= "<<???>>"
    ]
  where
    check c raw encoded = do
        review (optic c) raw @?= assertBase64 encoded
        view (optic c) (assertBase64 encoded) @?= raw
        construct c raw @?= assertBase64 encoded
        deconstruct c (assertBase64 encoded) @?= raw

untypedSuite :: (Eq a, Show a, IsString a) => String -> Optics a -> TestTree
untypedSuite name o = testGroup name
    [ testGroup "RFC 4648 vectors"
        [ testCase (show raw) $ do
            review (standardUntyped o) raw @?= padded
            preview (standardUntyped o) padded @?= Just raw
            review (urlUntyped o) raw @?= padded
            preview (urlUntyped o) padded @?= Just raw
            preview (urlUntyped o) bare @?= Just raw
            review (unpaddedUntyped o) raw @?= bare
            preview (unpaddedUntyped o) bare @?= Just raw
        | (raw, padded, bare) <- vectors
        ]
    , testCase "distinct alphabets" $ do
        review (standardUntyped o) "<<???>>" @?= "PDw/Pz8+Pg=="
        review (urlUntyped o) "<<???>>" @?= "PDw_Pz8-Pg=="
        preview (standardUntyped o) "PDw/Pz8+Pg==" @?= Just "<<???>>"
        preview (urlUntyped o) "PDw_Pz8-Pg==" @?= Just "<<???>>"
        reject (standardUntyped o) "PDw_Pz8-Pg=="
        reject (urlUntyped o) "PDw/Pz8+Pg=="
        reject (unpaddedUntyped o) "PDw/Pz8+Pg"
    , testGroup "malformed standard encoding"
        [ testCase (show bad) $ reject (standardUntyped o) bad
        | bad <- ["%", "Z", "Zg", "Zg=", "Zg===", "=m9v", "Zm=v", "Zh==", "Zm9=", "Zg==AAAA", "Zm9v\n"]
        ]
    , testGroup "malformed URL encoding"
        [ testCase (show bad) $ reject (urlUntyped o) bad
        | bad <- ["%", "Z", "Zg=", "Zg===", "Zh==", "Zh", "Zm9", "Zm9=", "Zg==AAAA", "Zm9v\n"]
        ]
    , testGroup "malformed unpadded URL encoding"
        [ testCase (show bad) $ reject (unpaddedUntyped o) bad
        | bad <- ["%", "Z", "Zg==", "Zm8=", "Zh", "Zm9", "Zg==AAAA", "Zm9v\n"]
        ]
    , testCase "URL matching accepts optional padding; review normalizes it" $
        fmap (review (urlUntyped o)) (preview (urlUntyped o) "Zg") @?= Just "Zg=="
    ]
  where
    -- Failed matches must preserve the original encoded input.
    reject p bad = matching p bad @?= Left bad

chunkedPrismTests :: TestTree
chunkedPrismTests = testGroup "Untyped lazy input across chunk boundaries"
    [ testCase "ByteString, valid input" $ do
        preview L._Base64Untyped (bytes "Zg==") @?= Just "f"
        preview L._Base64UrlUntyped (bytes "Zg==") @?= Just "f"
        preview L._Base64UrlUnpaddedUntyped (bytes "Zg") @?= Just "f"
    , testCase "ByteString, invalid suffix" $ do
        matching L._Base64Untyped (bytes "Zg==AAAA") @?= Left (bytes "Zg==AAAA")
        matching L._Base64UrlUntyped (bytes "Zg==AAAA") @?= Left (bytes "Zg==AAAA")
        matching L._Base64UrlUnpaddedUntyped (bytes "Zm9v%") @?= Left (bytes "Zm9v%")
    , testCase "Text, valid input" $ do
        preview V._Base64Untyped (text "Zg==") @?= Just "f"
        preview V._Base64UrlUntyped (text "Zg==") @?= Just "f"
        preview V._Base64UrlUnpaddedUntyped (text "Zg") @?= Just "f"
    , testCase "Text, invalid suffix" $ do
        matching V._Base64Untyped (text "Zg==AAAA") @?= Left (text "Zg==AAAA")
        matching V._Base64UrlUntyped (text "Zg==AAAA") @?= Left (text "Zg==AAAA")
        matching V._Base64UrlUnpaddedUntyped (text "Zm9v%") @?= Left (text "Zm9v%")
    ]
  where
    bytes = BL.fromChunks . map BS.singleton . BS.unpack
    text = TL.fromChunks . map T.singleton

roundtrips :: (Arbitrary b, Show b, Eq a, Show a) => String -> Optics a -> (b -> a) -> TestTree
roundtrips name o pack = testGroup name
    [ laws "standard" (standard o)
    , laws "padded URL" (url o)
    , laws "unpadded URL" (unpadded o)
    , laws "lenient, canonical input" (lenient o)
    , laws "lenient URL, canonical input" (urlLenient o)
    , prismLaws "untyped standard" (standardUntyped o)
    , prismLaws "untyped URL, canonical padded input" (urlUntyped o)
    , prismLaws "untyped unpadded URL" (unpaddedUntyped o)
    ]
  where
    prismLaws codecName p = testGroup codecName
        [ testProperty "preview . review" $ \input ->
            let raw = pack input
            in preview (clonePrism p) (review (clonePrism p) raw) === Just raw
        , testProperty "review . preview, canonical input" $ \input ->
            let encoded = review (clonePrism p) (pack input)
            in fmap (review (clonePrism p)) (preview (clonePrism p) encoded) === Just encoded
        ]
    laws codecName c = testGroup codecName
        [ testProperty "decode . encode" $ \input ->
            let raw = pack input in view (optic c) (review (optic c) raw) === raw
        , testProperty "encode . decode" $ \input ->
            let encoded = review (optic c) (pack input)
            in review (optic c) (view (optic c) encoded) === encoded
        , testProperty "pattern match . construct" $ \input ->
            let raw = pack input in deconstruct c (construct c raw) === raw
        ]

wrapperTests :: TestTree
wrapperTests = testGroup "Base64 wrapper"
    [ testCase "payload access does not decode or validate" $
        view Wrapped._Base64 (assertBase64 "%" :: Base64 'StdPadded BS.ByteString) @?= "%"
    , testCase "type-changing payload conversion preserves the alphabet" $ do
        let original = Base64.encodeBase64' "Sun"
            converted = over Wrapped._Base64 BL.fromStrict original :: Base64 'StdPadded BL.ByteString
        view L._Base64 converted @?= "Sun"
        extractBase64 converted @?= "U3Vu"
    , testCase "Wrapped and Rewrapped instances" $ do
        let original = Base64.encodeBase64' "Sun"
            converted = over _Wrapped SBS.toShort original :: Base64 'StdPadded SBS.ShortByteString
        view S._Base64 converted @?= "Sun"
        view _Wrapped' original @?= "U3Vu"
        (review _Wrapped' "U3Vu" :: Base64 'StdPadded BS.ByteString) @?= original
    , testProperty "wrap . unwrap" $ \bytes ->
        let value = assertBase64 (BS.pack bytes) :: Base64 'NonStandard BS.ByteString
        in review Wrapped._Base64 (view Wrapped._Base64 value) === value
    , testProperty "unwrap . wrap" $ \bytes ->
        let raw = BS.pack bytes
        in view Wrapped._Base64 (review Wrapped._Base64 raw :: Base64 'NonStandard BS.ByteString) === raw
    ]

alphabetTests :: TestTree
alphabetTests = testGroup "Alphabet prisms"
    [ testGroup name
        [ testCase "construct and match" $ preview (clonePrism p) (review (clonePrism p) ()) @?= Just ()
        , testCase "select exactly its constructor" $
            map (preview (clonePrism p)) [StdPadded, UrlPadded, UrlUnpadded, NonStandard] @?=
                [if i == target then Just () else Nothing | i <- [0 .. 3 :: Int]]
        ]
    | (name, p, target) <-
        [ ("StdPadded", Wrapped._StdPadded, 0)
        , ("UrlPadded", Wrapped._UrlPadded, 1)
        , ("UrlUnpadded", Wrapped._UrlUnpadded, 2)
        , ("NonStandard", Wrapped._NonStandard, 3)
        ] :: [(String, APrism' Alphabet (), Int)]
    ]

errorTests :: TestTree
errorTests = testGroup "Error prisms"
    [ testCase "construct and select decode error" $ do
        (_DecodeError # "invalid" :: Base64Error Int) @?= DecodeError "invalid"
        preview _DecodeError (DecodeError "invalid" :: Base64Error Int) @?= Just "invalid"
        preview _DecodeError (ConversionError 42 :: Base64Error Int) @?= Nothing
    , testCase "construct and select conversion error" $ do
        (_ConversionError # 42 :: Base64Error Int) @?= ConversionError 42
        preview _ConversionError (ConversionError 42 :: Base64Error Int) @?= Just 42
        preview _ConversionError (DecodeError "invalid" :: Base64Error Int) @?= Nothing
    , testCase "change conversion error type" $ do
        over _ConversionError show (ConversionError 42 :: Base64Error Int) @?= ConversionError "42"
        over _ConversionError show (DecodeError "invalid" :: Base64Error Int) @?= DecodeError "invalid"
    ]

byteStringOptics :: Optics BS.ByteString
byteStringOptics = Optics
    (Codec B._Base64 B.Base64 (\(B.Base64 a) -> a))
    (Codec B._Base64Url B.Base64Url (\(B.Base64Url a) -> a))
    (Codec B._Base64UrlUnpadded B.Base64UrlUnpadded (\(B.Base64UrlUnpadded a) -> a))
    (Codec B._Base64Lenient B.Base64Lenient (\(B.Base64Lenient a) -> a))
    (Codec B._Base64UrlLenient B.Base64UrlLenient (\(B.Base64UrlLenient a) -> a))
    B._Base64Untyped B._Base64UrlUntyped B._Base64UrlUnpaddedUntyped

lazyByteStringOptics :: Optics BL.ByteString
lazyByteStringOptics = Optics
    (Codec L._Base64 L.Base64 (\(L.Base64 a) -> a))
    (Codec L._Base64Url L.Base64Url (\(L.Base64Url a) -> a))
    (Codec L._Base64UrlUnpadded L.Base64UrlUnpadded (\(L.Base64UrlUnpadded a) -> a))
    (Codec L._Base64Lenient L.Base64Lenient (\(L.Base64Lenient a) -> a))
    (Codec L._Base64UrlLenient L.Base64UrlLenient (\(L.Base64UrlLenient a) -> a))
    L._Base64Untyped L._Base64UrlUntyped L._Base64UrlUnpaddedUntyped

shortByteStringOptics :: Optics SBS.ShortByteString
shortByteStringOptics = Optics
    (Codec S._Base64 S.Base64 (\(S.Base64 a) -> a))
    (Codec S._Base64Url S.Base64Url (\(S.Base64Url a) -> a))
    (Codec S._Base64UrlUnpadded S.Base64UrlUnpadded (\(S.Base64UrlUnpadded a) -> a))
    (Codec S._Base64Lenient S.Base64Lenient (\(S.Base64Lenient a) -> a))
    (Codec S._Base64UrlLenient S.Base64UrlLenient (\(S.Base64UrlLenient a) -> a))
    S._Base64Untyped S._Base64UrlUntyped S._Base64UrlUnpaddedUntyped

textOptics :: Optics T.Text
textOptics = Optics
    (Codec U._Base64 U.Base64 (\(U.Base64 a) -> a))
    (Codec U._Base64Url U.Base64Url (\(U.Base64Url a) -> a))
    (Codec U._Base64UrlUnpadded U.Base64UrlUnpadded (\(U.Base64UrlUnpadded a) -> a))
    (Codec U._Base64Lenient U.Base64Lenient (\(U.Base64Lenient a) -> a))
    (Codec U._Base64UrlLenient U.Base64UrlLenient (\(U.Base64UrlLenient a) -> a))
    U._Base64Untyped U._Base64UrlUntyped U._Base64UrlUnpaddedUntyped

lazyTextOptics :: Optics TL.Text
lazyTextOptics = Optics
    (Codec V._Base64 V.Base64 (\(V.Base64 a) -> a))
    (Codec V._Base64Url V.Base64Url (\(V.Base64Url a) -> a))
    (Codec V._Base64UrlUnpadded V.Base64UrlUnpadded (\(V.Base64UrlUnpadded a) -> a))
    (Codec V._Base64Lenient V.Base64Lenient (\(V.Base64Lenient a) -> a))
    (Codec V._Base64UrlLenient V.Base64UrlLenient (\(V.Base64UrlLenient a) -> a))
    V._Base64Untyped V._Base64UrlUntyped V._Base64UrlUnpaddedUntyped

shortTextOptics :: Optics TS.ShortText
shortTextOptics = Optics
    (Codec W._Base64 W.Base64 (\(W.Base64 a) -> a))
    (Codec W._Base64Url W.Base64Url (\(W.Base64Url a) -> a))
    (Codec W._Base64UrlUnpadded W.Base64UrlUnpadded (\(W.Base64UrlUnpadded a) -> a))
    (Codec W._Base64Lenient W.Base64Lenient (\(W.Base64Lenient a) -> a))
    (Codec W._Base64UrlLenient W.Base64UrlLenient (\(W.Base64UrlLenient a) -> a))
    W._Base64Untyped W._Base64UrlUntyped W._Base64UrlUnpaddedUntyped
