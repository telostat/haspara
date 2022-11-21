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

## Supported GHC Versions

1. `ghc90`
1. `ghc92`

At the moment, there is no particular reason for not supporting
`ghc94` except that required settings are not done yet in Nix support
files, in particular for Cabal dependency.

## Testing Against GHC Versions

You can use Nix support to test against different GHC versions:

```sh
nix-build --arg compiler "\"ghc92\""
nix-build --arg compiler "\"ghc90\""
```

`nix-build` command will default to `ghc90`. Check `./default.nix`
file for the default `ghc` version in case that this documentation is
out of date.

## Development

Before committing code to repository, reformat the code:

```sh
fourmolu -i src/ test/
```

Compile the codebase, check warnings and errors:

```sh
cabal build -O0
cabal test -O0
cabal haddock -O0
```

Run [hlint](https://github.com/ndmitchell/hlint):

```sh
hlint src/
```

Run [weeder](https://hackage.haskell.org/package/weeder):

```sh
weeder --require-hs-files
```

## Making Releases

1. Checkout `main` branch:

    ```sh
    git checkout main
    ```

2. Ensure that your branch is up to date:

    ```sh
    git pull
    ```

3. Update the `version` information in [package.yaml](./package.yaml) if
   required, run `hpack` to reflect the change on the `.cabal` file, and
   recompile the project, run tests and generate Haddock documentation:

    ```sh
    hpack
    cabal clean
    cabal build -O0
    cabal test -O0
    cabal haddock -O0
    nix-build --arg compiler "\"ghc92\""
    nix-build --arg compiler "\"ghc90\""
    ```

4. Update [CHANGELOG.md](./CHANGELOG.md) file:

    ```sh
    git-chglog --next-tag <NEW-VERSION> -o CHANGELOG.md
    ```

5. Commit, tag and push:

    ```sh
    git commit -am "chore(release): <NEW-VERSION>"
    git tag -a -m "Release <NEW-VERSION>" <NEW-VERSION>
    git push --follow-tags origin main
    ```

6. Create the package, upload to Hackage as a candidate first and check the result:

    ```sh
    cabal clean
    cabal build -O0
    cabal test -O0
    cabal haddock -O0
    cabal sdist
    cabal upload <path to .tar.gz archive>
    ```

7. If the candidate package release works fine, release to Hackage:

    ```sh
    cabal upload --publish <path to .tar.gz archive>
    ```

## License

Copyright Telostat Pte Ltd (c) 2021-2022.

This work is licensed under MIT license. See [LICENSE](./LICENSE).
