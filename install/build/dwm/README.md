# dwm build

A minimal build setup for dwm from suckless.org.

## Usage

```bash
./build-dwm.sh
```

Run from `install/build/dwm/`. This builds the checked-in `dwm-patched/`
source and stages the binary as `bin/dwm`, with a versioned copy and archive
in the same directory. To deploy it, run `./install-dwm.sh`; this replaces
the files in `/opt/dwm` and updates `/usr/local/bin/dwm`.

## Features
- **xmonadkeys** Custom keys similar to my xmonad

## Configuration

The checked-in `dwm-patched/config.mk` already contains the changes recorded
in `patches/dwm-bsd-holmen1.diff`; you do not need to apply it to build the
current source. The build script does not apply patches.

The patch changes build paths, not dwm behavior:

- `PREFIX` stages the build under `~/repos/dotfiles/install/build/dwm`, where
  the build and deploy scripts expect it.
- `X11INC` and `X11LIB` point to `/usr/local/include` and `/usr/local/lib`.
- `FREETYPEINC` points to `/usr/local/include/freetype2`.

These are FreeBSD-oriented paths. For Linux, check the installed X11 and
FreeType development files and adjust `dwm-patched/config.mk` as needed;
for example, Artix typically uses `/usr/include`, `/usr/lib`, and
`/usr/include/freetype2`. Keep the staging `PREFIX` for these scripts. Rebuild
on each OS; a FreeBSD binary is not a Linux binary.

At startup, dwm reads `TERMINAL` and `BROWSER` as executable names, falling
back to `xterm` and `firefox`. The selected terminal also runs `lf`.

## Patching
The patch file is a reference for reapplying these changes to fresh upstream
source. Commit `f456c3b` recorded both the patch and the corresponding
`config.mk` changes; it does not establish whether the patch command was run.
`fetch-source.sh` downloads source and a patch but does not apply patches or
replace `dwm-patched/`. When upgrading, inspect and reapply the relevant
patches to the new source:

```bash
./fetch-source.sh
```

For the BSD path patch, from the freshly downloaded source directory:

```bash
cd dwm-6.8
patch -p1 < ../patches/dwm-bsd-holmen1.diff
```

Some patches may fail if the source has changed, then review any .rej files and manually adjust the patch or source as needed.

`patches/dwm-env-commands-holmen1.diff` captures the environment-variable
changes for the existing patched source and its `config.def.h` (which defines
`filemgrcmd`). Apply it before generating `config.h`; an existing customized
`config.h` must be updated separately.

## My dwm Keybindings

| Keybinding | Function   |
|------------|------------|
| M-ENTER    | terminal   |
| M-Q        | kill window|
| M-M        | menu       |
| M-E        | filemanager|
| M-A        | applauncher|
| M-w        | browser    |
| M-X        | toggle xkb |
| M-S-Q      | exit       |

Based on [dwm](https://dwm.suckless.org/) from the suckless.org team.

## TODO

- [ ] xmonad keymap
- [ ] cleanup

