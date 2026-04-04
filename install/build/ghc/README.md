# GHC

## Download

Find versions from main [GHC site](https://www.haskell.org/ghc/)

**Note** if you use language server, check first
[ghc-version-support](https://haskell-language-server.readthedocs.io/en/latest/support/ghc-version-support.html)

More on hls below


## Build and Install

The default installation directory is ```/usr/local```

Run the configure script (as usual, run the script with --help to see what options it supports)

```bash
./configure
```
Ignore ```gcc: error: unrecognized command-line option '--target=x86_64-unknown-linux'```

Then run
```bash
sudo make install
```

For more information, full GHC documentation is available from the
main [GHC site](https://www.haskell.org/ghc/)

checking whether gcc used as a linker understands --target... gcc: error: unrecognized command-line option '--target=x86_64-unknown-linux'
no
checking whether gcc used as a linker understands --target... gcc: error: unrecognized command-line option '--target=x86_64-unknown-linux'

## HLS

Try from package managers, or

### Direct installation from Hackage

While possible via ```cabal install haskell-language-server```, is not recommended for most people. Said command builds the haskell-language-server binary and installs it in the default Cabal binaries folder, but the binary will only work with projects that use the same GHC version that built it.

**Note** add ```$HOME.local/bin``` to PATH
