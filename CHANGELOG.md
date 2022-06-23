<a name="unreleased"></a>
## [Unreleased]


<a name="0.0.0.5"></a>
## [0.0.0.5] - 2022-06-23
### Chore
- bump development version to 0.0.0.5

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
- **accounting:** delineate value and quantity concepts wrt Amount
- **accounting:** add Bounded instance to AccountKind
- **accounting:** add TrialBalance definitions
- **accounting:** add Amount and Balance definitions
- **accounting:** improve the Haspara.Accounting.Side module
- **accounting:** improve Balance module definitions

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
- init repository
- warn about unused packages during build
- add synopsis and category to package info
- update copyright notice
- init codebase
- **deps:** upgrade to lts-18.17
- **docs:** update README
- **release:** 0.0.0.0

### Feat
- add Aeson instances to Money data type

### Pull Requests
- Merge pull request [#2](https://github.com/telostat/haspara/issues/2) from telostat/release-chores
- Merge pull request [#1](https://github.com/telostat/haspara/issues/1) from telostat/init


[Unreleased]: https://github.com/telostat/haspara/compare/0.0.0.5...HEAD
[0.0.0.5]: https://github.com/telostat/haspara/compare/0.0.0.4...0.0.0.5
[0.0.0.4]: https://github.com/telostat/haspara/compare/0.0.0.3...0.0.0.4
[0.0.0.3]: https://github.com/telostat/haspara/compare/0.0.0.2...0.0.0.3
[0.0.0.2]: https://github.com/telostat/haspara/compare/0.0.0.1...0.0.0.2
[0.0.0.1]: https://github.com/telostat/haspara/compare/0.0.0.0...0.0.0.1
