# Changelog

All notable changes to this project are documented in this file.

## 0.4.1.0.9.14 — 2026-05-28

### Added

- **`in-place` plugin option**: When passed as a plugin argument, the plugin
  replaces the module's import declarations with the canonical import list
  directly in the source file, instead of writing a separate
  `<src-file>.full-imports` file.
