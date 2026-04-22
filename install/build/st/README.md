# st (simple terminal) build

A minimal build setup for st, the simple terminal from suckless.org.

## Usage

```bash
./build-st.sh
```

This will build and install to ```/bin``` with version tag

## Features
- **alpha** Background transparency (alpha = 0.85)
- **font2** Allows to add spare font besides default
- **scrollback**


## Configuration
The build uses the default configuration with patches applied.

## Patching
When upgrading st, download the new source version and reapply your patches

```bash
./fetch-source.sh
```

This will: Download st source and patches

```bash
patch -p1 < ../patches/st-xyz-n.n.n.diff
```

Some patches may fail if the source has changed, then review any .rej files and manually adjust the patch or source as needed.

### Custom Patches
- **st-scrollback-vim-G-holmen1.diff** - Adds `Shift+G` keybinding to jump to bottom of scrollback (Vim-like behavior)



## Installation

After building, install the binary to a system location:

```bash
# Create system directory
sudo mkdir -p /opt/st

# Copy binary to system location
sudo cp bin/st-0.9.2 /opt/st/

# Make it executable
sudo chmod +x /opt/st/st-0.9.2

# Create symlink in system PATH
sudo ln -sf /opt/st/st-0.9.2 /usr/local/bin/st
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
| Jump to bottom | `Shift+G` |
| **Special** |  |
| Break (send SIGINT) | `Ctrl+C` |
| Suspend process | `Ctrl+Z` |
| End of file | `Ctrl+D` |
| Clear screen | `Ctrl+L` |

Based on [st](https://st.suckless.org/) from the suckless.org team.



holmen1@besk debianinstall (debian14 *)$ fc-list | grep 'JetBrains Mono'
/home/holmen1/.local/share/fonts/JetBrainsMono-BoldItalic.ttf: JetBrains Mono:style=Bold Italic
/home/holmen1/.local/share/fonts/JetBrainsMono-SemiBold.ttf: JetBrains Mono,JetBrains Mono SemiBold:style=SemiBold,Regular
/home/holmen1/.local/share/fonts/JetBrainsMono-ExtraBoldItalic.ttf: JetBrains Mono,JetBrains Mono ExtraBold:style=ExtraBold Italic,Italic
/home/holmen1/.local/share/fonts/JetBrainsMono-MediumItalic.ttf: JetBrains Mono,JetBrains Mono Medium:style=Medium Italic,Italic
/home/holmen1/.local/share/fonts/JetBrainsMono-ExtraLightItalic.ttf: JetBrains Mono,JetBrains Mono ExtraLight:style=ExtraLight Italic,Italic
/home/holmen1/.local/share/fonts/JetBrainsMono-SemiBoldItalic.ttf: JetBrains Mono,JetBrains Mono SemiBold:style=SemiBold Italic,Italic
/home/holmen1/.local/share/fonts/JetBrainsMono-LightItalic.ttf: JetBrains Mono,JetBrains Mono Light:style=Light Italic,Italic
/home/holmen1/.local/share/fonts/JetBrainsMono-Light.ttf: JetBrains Mono,JetBrains Mono Light:style=Light,Regular
/home/holmen1/.local/share/fonts/JetBrainsMono-ExtraLight.ttf: JetBrains Mono,JetBrains Mono ExtraLight:style=ExtraLight,Regular
/home/holmen1/.local/share/fonts/JetBrainsMono-Thin.ttf: JetBrains Mono,JetBrains Mono Thin:style=Thin,Regular
/home/holmen1/.local/share/fonts/JetBrainsMono-Bold.ttf: JetBrains Mono:style=Bold
/home/holmen1/.local/share/fonts/JetBrainsMono-ThinItalic.ttf: JetBrains Mono,JetBrains Mono Thin:style=Thin Italic,Italic
/home/holmen1/.local/share/fonts/JetBrainsMono-ExtraBold.ttf: JetBrains Mono,JetBrains Mono ExtraBold:style=ExtraBold,Regular
/home/holmen1/.local/share/fonts/JetBrainsMono-Italic.ttf: JetBrains Mono:style=Italic
/home/holmen1/.local/share/fonts/JetBrainsMono-Medium.ttf: JetBrains Mono,JetBrains Mono Medium:style=Medium,Regular
/home/holmen1/.local/share/fonts/JetBrainsMono-Regular.ttf: JetBrains Mono:style=Regular
