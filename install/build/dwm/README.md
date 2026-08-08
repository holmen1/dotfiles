# dwm build

A minimal build setup for dwm from suckless.org.

## Usage

```bash
./build-dwm.sh
```

This will build and install to ```/bin``` with version tag

## Features
- **noborder** Remove the border when there is only one window visible 

## Configuration
The build uses the default configuration with patches applied.

## Patching [currently using default]
When upgrading dwm, download the new source version and reapply your patches

```bash
./fetch-source.sh
```

```bash
patch -p1 < ../patches/dwm-xyz-n.n.n.diff
```

Some patches may fail if the source has changed, then review any .rej files and manually adjust the patch or source as needed.

## My dwm Keybindings [currently using default]

| Keybinding | Function   |
|------------|------------|
| M-S-ENTER  | terminal   |
| M-S-Q      | exit   |

Based on [dwm](https://st.suckless.org/) from the suckless.org team.

## TODO

- [] xmonad keymap
- [] cleanup

