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

Before committing code to repository, reformat the code:

```sh
fourmolu -i src/ test/
```

Compile the codebase, check warnings and errors:

```sh
cabal build
cabal test
cabal haddock
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

1. Switch to `develop` branch:

    ```sh
    git checkout develop
    ```

1. Ensure that your development branch is up to date:

    ```sh
    git pull
    ```

1. Checkout `main` branch:

    ```sh
    git checkout main
    ```

1. Merge `develop` branch to `main`:

    ```sh
    git merge --no-ff develop
    ```

1. Update the `version` information in [package.yaml](./package.yaml) if
   required and recompile the project to reflect the change on the `.cabal`
   file:

    ```sh
    stack build
    ```

1. Update [CHANGELOG.md](./CHANGELOG.md) file:

    ```sh
    git-chglog --next-tag <NEW-VERSION> -o CHANGELOG.md
    ```

1. Commit, tag and push:

    ```sh
    git commit -am "chore(release): <NEW-VERSION>"
    git tag -a -m "Release <NEW-VERSION>" <NEW-VERSION>
    git push --follow-tags origin main
    ```

1. Release to Hackage as a candidate first and check the result:

    ```sh
    stack upload --candidate .
    ```

1. If the candidate package release works fine, release to Hackage:

    ```sh
    stack upload .
    ```

1. Checkout to `develop` and rebase onto `main`:

    ```sh
    git checkout develop
    git rebase main
    ```

1. Update the `version` information in [package.yaml](./package.yaml) with the
   upcoming version and recompile the project to reflect the change on the
   `.cabal` file:

   ```sh
   stack build
   ```

1. Commit and push:

    ```sh
    git commit -am "chore: bump development version to <UPCOMING-VERSION>"
    git push
    ```

## License

Copyright Telostat Pte Ltd (c) 2021-2022.

This work is licensed under MIT license. See [LICENSE](./LICENSE).
