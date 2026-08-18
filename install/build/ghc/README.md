# GHC

Glasgow Haskell Compiler — installed from binary distribution into

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
$ ./build-ghc.sh 9.14.1
[...]

The Glorious Glasgow Haskell Compilation System, version 9.14.1
/usr/local/bin/ghc
```

The script:
1. Downloads the binary tarball for `x86_64-deb12-linux` (suitable for Arch/Artix)
2. Installs into `/usr/local`

When `./configure`, this gcc error can be ignored
```
checking whether gcc used as a linker understands --target... gcc: error: unrecognized command-line option '--target=x86_64-unknown-linux'
no
```

## Local install

```bash
./configure --prefix=<dir> && sudo make install
```

## HLS

<!-- TODO: document HLS installation -->
