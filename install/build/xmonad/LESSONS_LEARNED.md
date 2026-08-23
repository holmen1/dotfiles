# LESSONS LEARNED

## 2026-08 GHC's implicit linking: the package env file is a linker script gcc never needs

### Cabal store ~= `/usr/lib` + ldconfig cache, but per-user and hash-addressed

`cabal install --lib xmonad-0.18.1 xmonad-contrib-0.18.2` builds both
packages and drops them into `~/.local/state/cabal/store/ghc-<ver>/`, one
directory per package, named `<name>-<version>-<hash>` (hash = full
dependency resolution, so two different builds of "the same" version
coexist without collision). This is the gcc-world equivalent of `make
install` populating `/usr/lib` - except versioned and hashed instead of
whatever-was-last-installed-wins.

Cabal also registers each package into a **package database** (`package.db`)
under that store dir - GHC's analogue of the `ldconfig` cache: an index
mapping package name/version to the actual files, so the compiler doesn't
need to be told exact paths every time.

### GHC has explicit flags for this - we're just not using them here

GHC's linking model does have a direct gcc parallel, if you want it:

| gcc                          | GHC                                    |
|-------------------------------|-----------------------------------------|
| `-L/path/to/libs`             | `-package-db /path/to/package.db`       |
| `-lfoo`                        | `-package foo` or `-package-id foo-1.0-<hash>` |
| default system lib dirs        | GHC's global + user package db (implicit) |
| `-nostdlib` / `-nodefaultlibs`  | `-hide-all-packages`                    |

So this *could* be written gcc-style, fully explicit, no generated file:

```sh
ghc --make xmonad.hs \
    -package-db "$STORE_DIR/package.db" \
    -package xmonad-0.18.1 \
    -package xmonad-contrib-0.18.2 \
    -o xmonad
```

That's exactly `-L` + `-l` per dependency

### The environment file is that flag list, externalized

`--package-env="$WORK_DIR"` makes cabal also write
`$WORK_DIR/.ghc.environment.x86_64-linux-<ghc-ver>`, and the real file has
more lines than just `package-id`:

```
clear-package-db
global-package-db
package-db /home/holmen1/.cabal/store/ghc-9.12.4/package.db
package-id base-4.21.2.0-b708
package-id xmonad-0.18.1-c8b7a5839caccdafd9aec4d51ca537398e363378faf3343c9d9036dd59379921
package-id xmonad-contrib-0.18.2-17b44d76fac2a9101f5bba2fb8e5114c49c60de8c09b64c1cfdc4007eaf65306
```

The first two lines set up the db *search stack* before any `-package-id`
is resolved - gcc equivalent of `-nostdlib` followed by manually re-adding
back `-L/usr/lib -lc`:

- `clear-package-db` = `-nostdlib`/`-nodefaultlibs`: drop GHC's implicit
  global+user db stack, don't trust ambient defaults.
- `global-package-db` = explicit `-L` for `/usr/lib`: re-add *only* GHC's
  own global db (where `base`, `ghc-prim`, etc. live) back onto the stack,
  by name, not by default inheritance.
- `package-db /home/holmen1/.../package.db` = `-L$STORE_DIR`: add the
  Cabal store's db as a second search location, for `xmonad`/`xmonad-contrib`.

Then every `package-id` line is one `-package-id` flag, resolved against
whichever db in that stack actually has it (`base` from the global db,
`xmonad`/`xmonad-contrib` from the store db) - the full stack is rebuilt
explicitly every time, not left to compiler defaults.

### The one thing with no gcc equivalent: cwd-triggered auto-loading

gcc never scans your current directory for a linker script. GHC does,
for exactly this file: if a `.ghc.environment.*` file exists in GHC's cwd,
it's read automatically, equivalent to every `-package-id` line in it
being typed on the command line. No flag enables this - it's cwd-implicit,
closer to a shell auto-sourcing `.envrc` than anything in the C toolchain.

This is why `build-custom-xmonad.sh` does `cd "$WORK_DIR"` before
`ghc --make "$ENV_DIR/xmonad.hs"`: the source path is absolute and
unaffected by cwd, but the environment-file pickup is cwd-only

### Flags worth noting

- `-fforce-recomp`: skip GHC's mtime-based recompilation check (its rough
  `make` equivalent) - always rebuild, no incremental-build speedup.
- `-outputdir DIR`: put all intermediate `.o`/`.hi` here, see above.
- `-Wall`: same meaning as gcc's `-Wall`.


## 2026-08 Upgrading libraries

### GHC

Unregister old versions, list installed, verify dependencies

```bash
$ ghc-pkg --help
  ghc-pkg unregister [pkg-id] 
    Unregister the specified packages in the order given

  ghc-pkg list [pkg]
    List registered packages in the global database, and also the
    user database

  ghc-pkg dot
    Generate a graph of the package dependencies

  ghc-pkg check
    Check the consistency of package dependencies and list broken packages
```

### Compare

```bash
$ ll xmonad-0.18.1-ghc-9.14.1
-rwxr-xr-x 1 holmen1 holmen1 6.2M Aug 17 19:16 xmonad-0.18.1-ghc-9.14.1
$ size xmonad-0.18.1-ghc-9.14.1
   text    data     bss     dec     hex filename
3808244  422640   30824 4261708  41074c xmonad-0.18.1-ghc-9.14.1
```

```bash
$ ll xmonad-0.18.1-ghc-9.12.2
-rwxr-xr-x 1 holmen1 holmen1 6.8M Aug 17 19:45 xmonad-0.18.1-ghc-9.12.2
$ size xmonad-0.18.1-ghc-9.12.2
   text    data     bss     dec     hex filename
4122953  483896   18536 4625385  4693e9 xmonad-0.18.1-ghc-9.12.2
```

## Hackage Package Upper Bounds vs GHC 9.12+

When building Haskell packages from Hackage tarballs with GHC 9.12+ (base 4.21),
many older packages have tight upper bounds that predate this GHC version. The
tarball ships the original `.cabal` with the old bounds — Hackage revisions
(which relax the bounds) are only available via `cabal get`, not in the tarball.

The workaround is to `sed`-patch the `.cabal` file after extraction, before
running `runhaskell Setup.lhs configure`. Dots must be escaped in the sed pattern (`\.`).

Known offenders in the xmonad dependency tree:

| Package | Constraint | Fix |
|---------|------------|-----|
| `setlocale-1.0.0.10` | `base >= 4.6 && <= 4.16` | remove upper bound |
| `splitmix-0.1.0.2` | `base >=4.3 && <4.16` | remove upper bound |
| `splitmix-0.1.0.2` | `deepseq >= 1.3.0.0 && <1.5` | remove upper bound |


## Examining XMonad Binaries to Understand Size Differences


### Basic Analysis

```bash
# Compare file types
file ~/.local/bin/xmonad
file ~/.cache/xmonad/xmonad-x86_64-linux

# See what libraries they depend on
ldd ~/.local/bin/xmonad
ldd ~/.cache/xmonad/xmonad-x86_64-linux

# Check section sizes
size ~/.local/bin/xmonad
size ~/.cache/xmonad/xmonad-x86_64-linux
```

### Looking at Debug Symbols

```bash
# Count symbols in each binary
nm ~/.local/bin/xmonad | wc -l
nm ~/.cache/xmonad/xmonad-x86_64-linux | wc -l

# Create a stripped copy to see impact of debug symbols
cp ~/.cache/xmonad/xmonad-x86_64-linux /tmp/xmonad-stripped
strip /tmp/xmonad-stripped
ls -la /tmp/xmonad-stripped
```

### Deeper Analysis

```bash
# Examine section headers
readelf -S ~/.local/bin/xmonad | grep -A2 "\[.*\] \."
readelf -S ~/.cache/xmonad/xmonad-x86_64-linux | grep -A2 "\[.*\] \."

# Check compilation flags (might show optimization level)
readelf -p .comment ~/.local/bin/xmonad
readelf -p .comment ~/.cache/xmonad/xmonad-x86_64-linux
```

```bash
cmp -l file1.bin file2 | wc -l          # How many differences?
cmp -l file1.bin file2 | head            # Where do they start?

# Then visualize
vbindiff file1 file2

#Or for human-readable
diff -u <(xxd -g1 -c 32 file1.bin) <(xxd -g1 -c 32 file2.bin) | less

# Pro tip: If these are ELF executables or object files, also try:bash
objdump -d file1 > 1.asm
objdump -d file2 > 2.asm
diff -u 1.asm 2.asm
```


These commands will help you understand:
1. Whether debug symbols are present (explaining larger size)
2. Which optimization levels were used 
3. Whether static vs dynamic linking differs
4. Which sections contribute to size differences

The cache binary is likely larger because it contains debug information to help with error reporting during development, whereas the installed binary may be optimized for size and performance.

## Note on Binary Locations

- Original installation: `~/.local/bin/xmonad`
- Recompiled configuration: `~/.cache/xmonad/xmonad-x86_64-linux`

XMonad automatically uses the newer cache version when available.

## GHC environment file

(`.ghc.environment.x86_64-linux-9.4.8`). This file is automatically generated by `cabal` when you use the `--package-env` option, and it defines the package environment for GHC (the Haskell compiler)

## Xmonad the Default Configuration

- **Mod Key**: The `Alt` key (`mod1Mask`) is used as the modifier key
- **Terminal**: Defaults to `xterm`
- **Key Bindings**:
  - `Mod + Shift + Enter`: Launches the terminal
  - `Mod + Shift + C`: Closes the focused window
  - `Mod + Space`: Switches between layouts
  - `Mod + Tab`: Cycles through windows
  - `Mod + Q`: Restarts xmonad

## cabal vs ghc

[TODO]
Investigate if cabal flags: +with-xft should be handled by ghc
```
holmen1@x1 bin (master)$ ldd xmonad-v0.18.1
        linux-vdso.so.1 (0x00007f854a245000)
        libm.so.6 => /usr/lib/libm.so.6 (0x00007f854a0f4000)
        libXft.so.2 => /usr/lib/libXft.so.2 (0x00007f854a0da000)
        libXss.so.1 => /usr/lib/libXss.so.1 (0x00007f854a0d5000)
        libXinerama.so.1 => /usr/lib/libXinerama.so.1 (0x00007f854a0d0000)
        libXext.so.6 => /usr/lib/libXext.so.6 (0x00007f854a0bc000)
        libX11.so.6 => /usr/lib/libX11.so.6 (0x00007f8549f78000)
        libXrandr.so.2 => /usr/lib/libXrandr.so.2 (0x00007f8549f6b000)
        libgmp.so.10 => /usr/lib/libgmp.so.10 (0x00007f8549ec4000)
        libc.so.6 => /usr/lib/libc.so.6 (0x00007f8549c00000)
        /lib64/ld-linux-x86-64.so.2 => /usr/lib64/ld-linux-x86-64.so.2 (0x00007f854a247000)
        libfontconfig.so.1 => /usr/lib/libfontconfig.so.1 (0x00007f8549e73000)
        libfreetype.so.6 => /usr/lib/libfreetype.so.6 (0x00007f8549b30000)
        libXrender.so.1 => /usr/lib/libXrender.so.1 (0x00007f8549e67000)
        libxcb.so.1 => /usr/lib/libxcb.so.1 (0x00007f8549e3a000)
        libexpat.so.1 => /usr/lib/libexpat.so.1 (0x00007f8549b03000)
        libz.so.1 => /usr/lib/libz.so.1 (0x00007f8549e1f000)
        libbz2.so.1.0 => /usr/lib/libbz2.so.1.0 (0x00007f8549af0000)
        libpng16.so.16 => /usr/lib/libpng16.so.16 (0x00007f8549ab5000)
        libbrotlidec.so.1 => /usr/lib/libbrotlidec.so.1 (0x00007f8549aa6000)
        libXau.so.6 => /usr/lib/libXau.so.6 (0x00007f8549aa1000)
        libXdmcp.so.6 => /usr/lib/libXdmcp.so.6 (0x00007f8549a99000)
        libbrotlicommon.so.1 => /usr/lib/libbrotlicommon.so.1 (0x00007f8549a76000)
holmen1@x1 bin (master)$ ldd xmonad-0.18.1
        linux-vdso.so.1 (0x00007f2c459da000)
        libm.so.6 => /usr/lib/libm.so.6 (0x00007f2c45889000)
        libXss.so.1 => /usr/lib/libXss.so.1 (0x00007f2c45884000)
        libXinerama.so.1 => /usr/lib/libXinerama.so.1 (0x00007f2c4587f000)
        libXext.so.6 => /usr/lib/libXext.so.6 (0x00007f2c4586b000)
        libX11.so.6 => /usr/lib/libX11.so.6 (0x00007f2c45729000)
        libXrandr.so.2 => /usr/lib/libXrandr.so.2 (0x00007f2c4571a000)
        libgmp.so.10 => /usr/lib/libgmp.so.10 (0x00007f2c45673000)
        libc.so.6 => /usr/lib/libc.so.6 (0x00007f2c45400000)
        /lib64/ld-linux-x86-64.so.2 => /usr/lib64/ld-linux-x86-64.so.2 (0x00007f2c459dc000)
        libxcb.so.1 => /usr/lib/libxcb.so.1 (0x00007f2c45648000)
        libXrender.so.1 => /usr/lib/libXrender.so.1 (0x00007f2c4563c000)
        libXau.so.6 => /usr/lib/libXau.so.6 (0x00007f2c45637000)
        libXdmcp.so.6 => /usr/lib/libXdmcp.so.6 (0x00007f2c4562d000)
```

## Why Export HOSTNAME is Necessary for XMonad
When lookupEnv "HOSTNAME" returns Nothing inside your xmonad.hs (line 54), it means the HOSTNAME environment variable isn't available to XMonad. This happens due to how environment variables are handled in desktop environments.

Environment Variable Inheritance
Environment variables are passed from parent processes to child processes. However, this inheritance chain is affected by how window managers like XMonad are launched:

When Using startx
With startx, a similar issue occurs:

The X server starts with a minimal environment
Only variables explicitly exported in .xinitrc or session scripts are available to XMonad
Even though HOSTNAME might be set in your shell, it doesn't automatically propagate
Solution
This is why you need to explicitly export HOSTNAME="xps" in:

Your .xinitrc file (for startx)
Your xmonad-session-rc script (for LightDM)
By explicitly exporting the variable in these startup scripts, you ensure it's available in XMonad's environment when lookupEnv "HOSTNAME" is called.

