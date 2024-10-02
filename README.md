# haspara

[![Hackage version](https://img.shields.io/hackage/v/haspara.svg?label=Hackage)](https://hackage.haskell.org/package/haspara)
![GitHub release (latest by date)](https://img.shields.io/github/v/release/telostat/haspara)
![GitHub contributors](https://img.shields.io/github/contributors/telostat/haspara)
![GitHub](https://img.shields.io/github/license/telostat/haspara)

> **Note:** This software is under development and of prototype quality at the
> moment. Expect significant breaking changes without notification until we
> reach the first minor version. Until then, we will keep bumping the patch
> version.

*haspara* is a Haskell library that provides monetary definitions and a
rudimentary (and experimental) accounting functionality.

## Development

Big, long build command for the impatient:

```sh
hpack &&
    direnv reload &&
    fourmolu -i src/ test/ &&
    prettier --write . &&
    find . -iname "*.nix" -not -path "*/nix/sources.nix" -print0 | xargs --null nixpkgs-fmt &&
    hlint src/ test/ &&
    cabal build -O0 &&
    cabal run -O0 haspara -- --version &&
    cabal v1-test &&
    cabal haddock -O0
```

To test and build codebase in development environment, run:

```sh
dev-test-build
```

## License

Copyright Telostat Pte Ltd (c) 2021-2024.

This work is licensed under MIT license. See [LICENSE](./LICENSE).
