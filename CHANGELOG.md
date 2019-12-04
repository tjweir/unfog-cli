# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- Add basic help command [#3]

### Fixed

- Delete command [#1]
- Worktime doesn't take in consideration the context [#2]

## [0.2.0] - 2019-12-01

### Added 

- Changelog
- New syntax to remove tags: `-tag` [#6]

### Changed

- **[BREAKING]** New parser introduced, based on parser combinators ([ReadP](https://hackage.haskell.org/package/base-4.12.0.0/docs/Text-ParserCombinators-ReadP.html)). Events have been impacted (simplified), so the previous store is not compatible anymore. You can upgrade it with this command: `sed -i -E 's/True |False |\+//g' ~/.config/unfog/store`

### Fixed 

- Create config dir if not exists [#5]

## [0.1.4] - 2019-11-26

### Added

- Installation script

### Changed

- Binaries compression (tar.gz)

## [0.1.3] - 2019-11-26

### Changed

- Build optimization

## [0.1.2] - 2019-11-26

### Added

- Travis CI
- Cross-compilation to Linux, OSX and Windows

## [0.1.1] - 2019-11-24

### Changed

- Update README and LICENSE

## [0.1.0] - 2019-11-23

First release :tada:

### Added

- Linux binaries

[unreleased]: https://github.com/unfog-io/unfog-cli/compare/v0.2.0...HEAD
[0.2.0]: https://github.com/unfog-io/unfog-cli/compare/v0.1.4...v0.2.0
[0.1.4]: https://github.com/unfog-io/unfog-cli/compare/v0.1.3...v0.1.4
[0.1.3]: https://github.com/unfog-io/unfog-cli/compare/v0.1.2...v0.1.3
[0.1.2]: https://github.com/unfog-io/unfog-cli/compare/v0.1.1...v0.1.2
[0.1.1]: https://github.com/unfog-io/unfog-cli/compare/v0.1.0...v0.1.1
[0.1.0]: https://github.com/unfog-io/unfog-cli/releases/tag/v0.1.0

[#1]: https://github.com/unfog-io/unfog-cli/issues/1
[#2]: https://github.com/unfog-io/unfog-cli/issues/2
[#3]: https://github.com/unfog-io/unfog-cli/issues/3
[#5]: https://github.com/unfog-io/unfog-cli/issues/5
[#6]: https://github.com/unfog-io/unfog-cli/issues/6
