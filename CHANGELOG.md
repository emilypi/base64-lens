# Revision history for base64-lens

## 0.4 — 2026-10-13

* Support `base64` 2.0 and GHC 9.6–9.14; update dependency bounds for
  `bytestring` 0.11–0.12, `text` 2.0–2.2, and `lens` 5.0–5.3.
* Change string optics from raw-string prisms to typed isomorphisms over
  `Base64` wrappers. Encoding records the alphabet; decoding consumes the
  corresponding wrapper. Validate untrusted standard input with `parseBase64`.
* Add `_Base64Untyped`, `_Base64UrlUntyped`, and
  `_Base64UrlUnpaddedUntyped` alongside the typed optics in all six string
  modules. These restore raw-string prisms using the checked untyped decoders.
* Change all string patterns to construct and match typed wrappers. Lenient
  optics and patterns use `NonStandard`; fix their patterns to encode on
  construction and decode on matching.
* Add `Data.Base64.Types.Lens` with a type-changing payload isomorphism,
  `Wrapped`/`Rewrapped` instances, and prisms for all `Alphabet` constructors.
  Payload optics preserve the alphabet index and do not validate or encode.
* Generalize `_ConversionError` to allow changing the error payload type.
* Simplify the string optics to direct compositions of the typed codecs.
  Use `Trustworthy` because `base64` 2.0's encoding modules are not Safe
  Haskell modules; the error optics remain `Safe`.
* Add regression and property tests for all representations, typed patterns,
  wrapper optics, alphabet constructors, untyped decoding and rejection, lazy
  chunk boundaries, binary data, Unicode, and errors.
  Replace the unused doctest stub.
* Refresh CI to test source distributions across supported compilers and both
  scalar and SIMD decoding backends.

### Migration from 0.3

For typed decoding, validate external standard-alphabet input with
`parseBase64`, then use `view` or `(^.)` with the corresponding typed optic.
Use `extractBase64` or `Data.Base64.Types.Lens._Base64` when an API needs the
encoded payload without its wrapper.

To keep using `preview` or `(^?)` with ordinary encoded strings, rename the
prisms:

| Previous name | Replacement |
| --- | --- |
| `_Base64` | `_Base64Untyped` |
| `_Base64Url` | `_Base64UrlUntyped` |
| `_Base64UrlUnpadded` | `_Base64UrlUnpaddedUntyped` |

The generic URL prism accepts both padded and unpadded input and reviews to
padded output. The unpadded URL prism rejects padding. Text decoding continues
to require UTF-8 payloads; use the ByteString optics for arbitrary binary data.

## 0.3.1


* Add lazy and short text and bytestring optics
* Add safe/trustworthy pragmas
* Migrate to Github CI
* Update nix derivation

## 0.3

* Instead of focusing on the `Base64Lenient` `Iso` as a `Lens` (which swaps its focus compared to its sibling `Prism`s),
  change it to be symmetrical with the other `Prism`s so that it can be used as a generalized `Prism`.

## 0.2

* Bump `base64` dependency to settle on 0.4 with the "finalized" api.
* Remove `_Base64Unpadded` prism and `Base64Unpadded` patterns from `ByteString` and `Text`
* Kill all CPP

## 0.1.0.0

* First version. Released on an unsuspecting world.
