# Changelog

All notable changes to this project are documented in this file.

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
