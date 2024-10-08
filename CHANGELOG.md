# Changelog

<a name="unreleased"></a>
## [Unreleased]


<a name="0.0.0.10"></a>
## [0.0.0.10] - 2024-10-07
### Fix
- **package:** add license-file descriptor to package.yaml


<a name="0.0.0.9"></a>
## [0.0.0.9] - 2024-10-07
### Chore
- reformat changelog configuration file
- **release:** 0.0.0.9
- **release:** setup and document release process

### Docs
- add GitHub release step to "Making Releases" section in README

### Fix
- **deps:** update dependency boundaries, adopt new Nix shell, chores

### Test
- add GitHub action for testing

### Pull Requests
- Merge pull request [#20](https://github.com/telostat/haspara/issues/20) from telostat/19-find-a-solution-to-automate-releases
- Merge pull request [#18](https://github.com/telostat/haspara/issues/18) from telostat/vst/revisit-nix-upgrade-deps


<a name="0.0.0.8"></a>
## [0.0.0.8] - 2022-11-21
### Chore
- bump development version to 0.0.0.8
- **nix:** make it more convenient to test against different ghc versions
- **nix:** make default haspara Nix package come with haddock
- **release:** 0.0.0.8

### Fix
- add explicit Aeson.ToJSON.toEncoding implementations
- **deps:** support ghc92

### Pull Requests
- Merge pull request [#17](https://github.com/telostat/haspara/issues/17) from telostat/15-make-it-more-convenient-to-test-against-different-ghc-versions
- Merge pull request [#16](https://github.com/telostat/haspara/issues/16) from telostat/vst/aeson-encoding


<a name="0.0.0.7"></a>
## [0.0.0.7] - 2022-11-17
### Chore
- bump development version to 0.0.0.7
- **dev:** document pre-release checks
- **release:** 0.0.0.7

### Docs
- add missing Haddock documentation for function

### Test
- fix tests for updated Balance data definition

### Pull Requests
- Merge pull request [#14](https://github.com/telostat/haspara/issues/14) from telostat/vst/test-and-doc-fixes


<a name="0.0.0.6"></a>
## [0.0.0.6] - 2022-11-17
### Chore
- remove stylish-haskell configuration
- drop Stack usage
- bump development version to 0.0.0.6
- **deps:** park library dependencies under library section
- **docs:** update development and release procedure in README
- **nix:** use telos.nix
- **release:** 0.0.0.6
- **test:** get ready for hspec-based testing

### Feat
- integrate inventory accounting into posting
- add division operations for `Quantity` type

### Fix
- **test:** revisit doctests, adopt 9.x TH changes

### Refactor
- start working on inventory machinery
- reorder language pragmas
- drop Haskell 2021 Extensions in favour of explicit pragmas
- reformat codebase using fourmolu

### Pull Requests
- Merge pull request [#13](https://github.com/telostat/haspara/issues/13) from telostat/vst/pnl-capture
- Merge pull request [#12](https://github.com/telostat/haspara/issues/12) from telostat/develop


<a name="0.0.0.5"></a>
## [0.0.0.5] - 2022-06-23
### Chore
- bump development version to 0.0.0.5
- **release:** 0.0.0.5

### Feat
- **accounting:** add Aeson instances to Haspara.Accounting.Journal module

### Pull Requests
- Merge pull request [#11](https://github.com/telostat/haspara/issues/11) from telostat/vst/add-missing-aeson-instances


<a name="0.0.0.4"></a>
## [0.0.0.4] - 2022-06-22
### Chore
- fix doctests and Haddock documentation
- start working on Haspara.Accounting.Journal module
- add new functions to Haspara.Quantity module
- bump development version to 0.0.0.4
- **release:** 0.0.0.4

### Feat
- add Haspara.Accounting.Side module
- **accounting:** improve Ledger module definitions
- **accounting:** improve Balance module definitions
- **accounting:** delineate value and quantity concepts wrt Amount
- **accounting:** add Bounded instance to AccountKind
- **accounting:** add TrialBalance definitions
- **accounting:** add Amount and Balance definitions
- **accounting:** improve the Haspara.Accounting.Side module

### Refactor
- **accounting:** remove Event definitions, revisit exports
- **accounting:** revisit Ledger{Entry} and Journal{Entry,EntryItem}

### Pull Requests
- Merge pull request [#10](https://github.com/telostat/haspara/issues/10) from telostat/vst/issue-9


<a name="0.0.0.3"></a>
## [0.0.0.3] - 2022-06-17
### Chore
- drop deriving-aeson dependency
- adopt Stack lts-19.11 (and GHC902), add Nix Shell
- bump development version to 0.0.0.3
- **build:** specify dependency version ranges
- **release:** 0.0.0.3

### Refactor
- revisit Haspara.FxQuote module, add new definitions

### Pull Requests
- Merge pull request [#6](https://github.com/telostat/haspara/issues/6) from telostat/chores-fixes-improvements
- Merge pull request [#7](https://github.com/telostat/haspara/issues/7) from telostat/vst/ghc902


<a name="0.0.0.2"></a>
## [0.0.0.2] - 2022-03-03
### Chore
- bump development version to 0.0.0.2
- **dev:** add Weeder configuration
- **dev:** add HLint configuration
- **dev:** produce .hie files during compilation
- **docs:** update README.md, fix haddock warnings
- **release:** 0.0.0.2

### Refactor
- revisit Haspara.Accounting module and its submodules
- refactor and move definitions from Haspara.Money to Haspara.Monetary
- revisit Haspara.FxQuote module
- revisit Haspara.Quantity module
- revisit Haspara.Currency module
- adopt Haskell 2021 extensions
- move definitions from Haspara.Internal.* to Haspara.*
- remove Date type in favour of Day type
- remove Id type and related definitions

### Pull Requests
- Merge pull request [#5](https://github.com/telostat/haspara/issues/5) from telostat/slim-down


<a name="0.0.0.1"></a>
## [0.0.0.1] - 2022-03-01
### Chore
- bump development version to 0.0.0.1
- **deps:** upgrade to lts-18.27
- **release:** 0.0.0.1

### Feat
- add rudimentary accounting functionality

### Pull Requests
- Merge pull request [#4](https://github.com/telostat/haspara/issues/4) from telostat/upgrade-stack-lts
- Merge pull request [#3](https://github.com/telostat/haspara/issues/3) from telostat/rudimentary-accounting


<a name="0.0.0.0"></a>
## 0.0.0.0 - 2022-03-01
### Chore
- integrate git-chglog
- warn about unused packages during build
- add synopsis and category to package info
- update copyright notice
- init codebase
- init repository
- **deps:** upgrade to lts-18.17
- **docs:** update README
- **release:** 0.0.0.0

### Feat
- add Aeson instances to Money data type

### Pull Requests
- Merge pull request [#2](https://github.com/telostat/haspara/issues/2) from telostat/release-chores
- Merge pull request [#1](https://github.com/telostat/haspara/issues/1) from telostat/init


[Unreleased]: https://github.com/telostat/haspara/compare/0.0.0.10...HEAD
[0.0.0.10]: https://github.com/telostat/haspara/compare/0.0.0.9...0.0.0.10
[0.0.0.9]: https://github.com/telostat/haspara/compare/0.0.0.8...0.0.0.9
[0.0.0.8]: https://github.com/telostat/haspara/compare/0.0.0.7...0.0.0.8
[0.0.0.7]: https://github.com/telostat/haspara/compare/0.0.0.6...0.0.0.7
[0.0.0.6]: https://github.com/telostat/haspara/compare/0.0.0.5...0.0.0.6
[0.0.0.5]: https://github.com/telostat/haspara/compare/0.0.0.4...0.0.0.5
[0.0.0.4]: https://github.com/telostat/haspara/compare/0.0.0.3...0.0.0.4
[0.0.0.3]: https://github.com/telostat/haspara/compare/0.0.0.2...0.0.0.3
[0.0.0.2]: https://github.com/telostat/haspara/compare/0.0.0.1...0.0.0.2
[0.0.0.1]: https://github.com/telostat/haspara/compare/0.0.0.0...0.0.0.1
