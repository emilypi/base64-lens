Base64-lens


This package provides optics and convenient pattern synonyms for the [base64](https://hackage.haskell.org/package/base64) library.

### Patterns

The pattern synonyms provided in this library are:

```haskell
pattern Base64 :: ByteString -> ByteString
pattern Base64Url :: ByteString -> ByteString
pattern Base64Unpadded :: ByteString -> ByteString
pattern Base64UrlUnpadded :: ByteString -> ByteString

-- and

pattern Base64 :: Text -> Text
pattern Base64Url :: Text -> Text
pattern Base64Unpadded :: Text -> Text
pattern Base64UrlUnpadded :: Text -> Text
```

These provide a convenient high level interface for passing Base64 encoded values.


### Optics

`Prism`s for encoding and decoding `Text` and `ByteString` values are given as part of the library:


```haskell
_Base64 :: Prism' ByteString ByteString
_Base64Url :: Prism' ByteString ByteString
_Base64Unpadded :: Prism' ByteString ByteString
_Base64UrlUnpadded :: Prism' ByteString ByteString

-- and

_Base64 :: Prism' Text Text
_Base64Url :: Prism' Text Text
_Base64Unpadded :: Prism' Text Text
_Base64UrlUnpadded :: Prism' Text Text

```

If a particular structure has a `Lens` into some `Text` or `ByteString` value they might want to encode (or decode), then composing such a `Lens` with these `Prisms` yields an affine `Traversal`, resulting in a structure which has the focus of its `Lens` encoded as or decoded from Base64(-url). All one needs to do is compose their optics:

```haskell

data MyStruct = MyStruct
  { _a :: Int
  , _b :: Text
  } deriving Show

b :: Lens' MyStruct Text
b = lens _b (\t b_ -> t { _b = b_ })

myB64Struct :: Traversal' s Text
myB64Struct = b . _Base64

-- >>> MyStruct 3 "U3Vu" ^? b . _Base64
-- MyStruct {_a = 3, _b = "Sun"}

bRe :: Review MyStruct Text
bRe = unto (\b -> MyStruct 0 b)

-- >>> bRe . _Base64 # "Sun"
-- MyStruct {_a = 0, _b = "UV3u"}
```

The data of a `Prism` naturally conforms to this "encoding/decoding" dichotomy, where the `Review`, or "builder" half of the `Prism` of type `b -> t` is an encoding, and the "Matcher" half of the prism, of type `s -> Either t a`, represents a decoding of a similar structure. Hence, `Prism` is the most appropriate structure.
