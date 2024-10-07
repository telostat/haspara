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
    nix-shell --run "dev-test-build -c"
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
    nix-shell --run "dev-test-build -c"
    nix-shell --run "cabal sdist"
    nix-shell --run "cabal upload dist-newstyle/sdist/haspara-<VERSION>.tar.gz"
    ```

7. If the candidate package release works fine, release to Hackage:

    ```sh
    nix-shell --run "cabal upload --publish dist-newstyle/sdist/haspara-<VERSION>.tar.gz"
    ```

8. Make sure that the release is available on [Hackage](https://hackage.haskell.org/package/haspara).
9. Make sure that the release is available on [GitHub](https://github.com/telostat/haspara/releases).

## License

Copyright Telostat Pte Ltd (c) 2021-2024.

This work is licensed under MIT license. See [LICENSE](./LICENSE).
