# Changelog for `vary`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

## 0.1.1.1 - 2025-02-06

- Loosen test dependency bound on QuickCheck.

## 0.1.1.0 - 2025-02-05

- Add serialization support of `Vary` for `binary`'s `Data.Binary` and `cereal`'s `Data.Serialize`. (c.f. [#6](https://github.com/Qqwy/haskell-vary/pull/6)) Thank you very much, @jmorag!
- Document `Data.Aeson` round-tripping behaviour (namely: only round-trippable if encodings do not overlap, the `UntaggedValue` sum encoding).

## 0.1.0.5 - 2025-02-05

- Relax max allowed versions of DeepSeq (<1.6), Hashable (<1.6), QuickCheck (<2.16)

## 0.1.0.4 - 2024-01-15

- Improved test coverage for exception cases; README is now fully tested using Literate Haskell.

## 0.1.0.3 - 2024-01-14

- Fix a typographic error in the documentation

## 0.1.0.2 - 2024-01-14

- Improve documentation

## 0.1.0.0 - 2024-01-13

- Initial version
