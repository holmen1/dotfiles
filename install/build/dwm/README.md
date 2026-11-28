# dwm build

A minimal build setup for dwm from suckless.org.

## Usage

```bash
./build-dwm.sh
```

This will build and install to ```/bin``` with version tag

## Features
- **xmonadkeys** Custom keys similar to my xmonad

## Configuration
**NOTE** FreeBSD: edit `X11INC` `X11LIB` `FREETYPEINC`

The build uses the default configuration with 1 patch applied.
At startup, dwm reads `TERMINAL` and `BROWSER` as executable names, falling
back to `xterm` and `firefox`. The selected terminal also runs `lf`.

## Patching
When upgrading dwm, download the new source version and reapply your patches

```bash
./fetch-source.sh
```

```bash
patch -p1 < ../patches/dwm-xyz-n.n.n.diff
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

