# st (simple terminal) build

A minimal build setup for st, the simple terminal from suckless.org.

## Usage

```bash
./build-st.sh
```

This will:
- Download st source (version 0.9.2)
- Download and apply patches directly
- Build and install to ~/.local/bin/st with version tag

## Features
- **Background transparency** (alpha = 0.85)
- **Scrollback** with keyboard and mouse
- Version-tagged builds

## Configuration
The build uses the default configuration with patches applied. To inspect or customize:

```bash
# View the current configuration
less ~/repos/dotfiles/install/build/st/st-0.9.2/config.h

# To customize for future builds, edit build-st.sh to modify config.h after patching
```

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
| **Scrollback** |  |
| Scroll up | `Shift+Page Up` |
| Scroll down | `Shift+Page Down` |
| **Special** |  |
| Break (send SIGINT) | `Ctrl+C` |
| Suspend process | `Ctrl+Z` |
| End of file | `Ctrl+D` |
| Clear screen | `Ctrl+L` |

Based on [st](https://st.suckless.org/) from the suckless.org team.
