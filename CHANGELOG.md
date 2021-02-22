# Revision history for base64-lens

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
