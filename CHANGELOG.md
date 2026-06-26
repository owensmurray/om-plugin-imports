# Changelog

All notable changes to this project are documented in this file.

## 0.4.2.0.9.14 — 2026-06-26

### Added

- **Import formatting module**: Generated import rendering now lives in
  `OM.Plugin.Imports.Format`, following simformat-style conventions:
  stable sorting, grouped parent/child entries, and 80-column wrapping.

### Fixed

- **Pattern keyword placement**: Only top-level imported pattern synonyms
  are prefixed with `pattern`; nested names in parenthesized import lists
  use the bare symbol.

- **Explicit empty import lists**: User-written imports such as
  `import M ()` are preserved in generated output even when no symbols
  are used from M. This matters for orphan instances and other
  side-effect imports.

## 0.4.1.1.9.14 — 2026-05-31

### Fixed

- **Non-deterministic import ordering**: Parent and child import names are
  now sorted by their rendered text at print time. GHC's `Name` `Ord`
  instance uses `nonDetCmpUnique`, so `Set.toAscList` and `Map` key
  order could vary between compilations.

## 0.4.1.0.9.14 — 2026-05-28

### Added

- **`in-place` plugin option**: When passed as a plugin argument, the plugin
  replaces the module's import declarations with the canonical import list
  directly in the source file, instead of writing a separate
  `<src-file>.full-imports` file.
