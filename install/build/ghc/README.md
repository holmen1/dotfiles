# GHC

Glasgow Haskell Compiler — installed from binary distribution into `/usr/local`.

Find versions from the main [GHC site](https://www.haskell.org/ghc/).

**Note:** if you use a language server, check
[ghc-version-support](https://haskell-language-server.readthedocs.io/en/latest/support/ghc-version-support.html)
before choosing a version.

Shared build conventions: [docs/build.md](../../../docs/build.md)

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

Haskell Language Server (HLS) must match your GHC version. Start with the
support matrix:
[ghc-version-support](https://haskell-language-server.readthedocs.io/en/latest/support/ghc-version-support.html)

### Install options

#### Option A: ghcup (recommended)

```bash
ghcup install hls
ghcup set hls latest
```

#### Option B: build from source via cabal

```bash
cabal update
cabal install haskell-language-server
```

### Verify

```bash
haskell-language-server-wrapper --version
haskell-language-server --version
```

### Troubleshooting

- **Unsupported GHC version:** choose a supported GHC from the matrix, or install
  an HLS version that explicitly supports your compiler.
- **Editor still can’t find HLS:** ensure `haskell-language-server-wrapper` is on
  `PATH` in the environment where the editor starts.
- **Multiple GHC installs:** run `ghc --version` and `which ghc` in the same shell
  your editor uses, then align HLS to that compiler.
