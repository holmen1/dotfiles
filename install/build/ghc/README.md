# GHC

Glasgow Haskell Compiler — installed from binary distribution into `~/.local/ghc-<version>`.

Find versions from the main [GHC site](https://www.haskell.org/ghc/).

**Note:** if you use a language server, check
[ghc-version-support](https://haskell-language-server.readthedocs.io/en/latest/support/ghc-version-support.html)
before choosing a version.

## Usage

```bash
./build-ghc.sh <version>
```

e.g.
```bash
./build-ghc.sh 9.12.4
```

The script:
1. Downloads the binary tarball for `x86_64-deb12-linux` (suitable for Arch/Artix)
2. Installs into `~/.local/ghc-<version>` via `./configure --prefix`

When `./configure`, this gcc error can be ignored
```
checking whether gcc used as a linker understands --target... gcc: error: unrecognized command-line option '--target=x86_64-unknown-linux'
no
```

## Activation

To make a version the system default:
```bash
./configure && sudo make install
```
installs to `/usr/local`

## HLS

<!-- TODO: document HLS installation -->
