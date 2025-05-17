# st (simple terminal) build

A minimal build setup for st, the simple terminal from suckless.org.

## Usage

```bash
./build-st.sh
```

This will:
- Download st source (version 0.9.2)
- Apply patches from `patches/`
- Apply custom configuration from `config.h`
- Build and install to `~/.local/bin/st`

## Customization

- Edit `config.h` to change appearance and behavior
- Add `.diff` files to `patches/` for additional features
```bash
# Download patches
cd patches/
curl -O https://st.suckless.org/patches/alpha/st-alpha-20220206-0.8.5.diff


```

## Current Features

- JetBrainsMono Nerd Font with fallbacks
- Background transparency (alpha = 0.85)

## st Keybindings


| Function | Keybinding |
|----------|------------|
| **Copy/Paste** |  |
| Copy | `Ctrl+Shift+C` |
| Paste | `Ctrl+Shift+V` |
| **Font Size** |  |
| Zoom in | `Ctrl+Shift+Page Up` |
| Zoom out | `Ctrl+Shift+Page Down` |
| Reset zoom | `Ctrl+Shift+Home` |
| **Special** |  |
| Break (send SIGINT) | `Ctrl+C` |
| Suspend process | `Ctrl+Z` |
| End of file | `Ctrl+D` |
| Clear screen | `Ctrl+L` |





Based on [st](https://st.suckless.org/) from the suckless.org team.