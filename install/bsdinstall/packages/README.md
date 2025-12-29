
# PKGLIST

## Usage
```bash
# Update all packages
sudo pkg upgrade

# Install a new package
sudo pkg install <package-name>

# Search for packages
pkg search <keyword>

# Remove a package
sudo pkg delete <package-name>

# Remove a package and its unused dependencies (recommended)
sudo pkg autoremove

# Clean package cache
sudo pkg clean

# Show package information
pkg info <package-name>

# List installed packages
pkg info

# Check for security advisories
pkg audit -F
```

## Package Management Philosophy
- **Use `pkg` for binary packages** - Fast, tested, recommended for most software
- **Use ports for custom builds** - When you need specific options or software unavailable as packages (see [../../README.md](../../README.md#building-from-ports-for-customlatest-builds))
- **Keep it minimal** - FreeBSD doesn't bloat by default; install only what you need

## Descriptions

### Core System
- `FreeBSD-set-base`, `FreeBSD-set-devel`, `FreeBSD-set-minimal`: Essential FreeBSD base system
- `FreeBSD-kernel-generic`, `FreeBSD-kernel-generic-dbg`: Kernel and debugging symbols
- `FreeBSD-set-src`: System sources (for ports and kernel development)
- `FreeBSD-set-lib32`: 32-bit libraries (for compatibility)
- `bash`: Bourne Again Shell (set as user shell via `chsh`)
- `sudo`: Execute commands as root
- `ca_root_nss`: Root SSL certificates
- `pkg`: Package manager
- `portconfig`: Configure port build options

### Hardware & Drivers
- `drm-kmod`: Direct Rendering Manager kernel modules (modern GPU support)
- `wifi-firmware-iwlwifi-kmod-7000`: Intel WiFi firmware (for Intel wireless cards)

### Display Server & Graphics
- `xlibre-minimal`: XLibre server (X.Org fork, built from ports)
- `xlibre-xf86-video-intel`: Intel graphics driver for XLibre
- `xsetroot`: Set X root window properties
- `xterm`: Terminal emulator for X

### Window Manager & Desktop Utilities
- `dmenu`: Dynamic menu for X
- `i3lock`: Screen locker
- `xbindkeys`: Keybinding daemon
- `xclip`: Clipboard integration for X
- `xcompmgr`: Simple X compositor (transparency, shadows)
- `feh`: Lightweight image viewer (wallpapers)
- `scrot`: Screenshot tool
- `dunst`: Lightweight notification daemon
- `setxkbmap`: Set X keyboard layout

### Development Tools
- `git`: Version control
- `gcc`: GNU Compiler Collection
- `gdb`: GNU Debugger
- `gmake`: GNU Make (many projects require this instead of BSD make)
- `llvm`: LLVM compiler infrastructure
- `bison`: Parser generator (required by some build processes)
- `pkgconf`: Package config tool (pkg-config alternative)

### Haskell Development (for XMonad)
- `ghc`: Glasgow Haskell Compiler
- `hs-cabal-install`: Haskell package manager
- `hs-haskell-language-server`: LSP server for Haskell

### Editors & LSP
- `neovim`: Modern Vim fork
- `lua-language-server`: LSP for Lua (Neovim config)
- `stylua`: Lua formatter
- `tree-sitter-cli`: Parser generator for syntax highlighting

### Utilities
- `fd`: Fast find alternative
- `ripgrep`: Fast grep alternative
- `tree`: Display directory structure
- `lf`: Terminal file manager
- `htop`: Interactive process viewer
- `fastfetch`: System information tool
- `wget`: Network downloader
- `ncurses`: Terminal UI library

### Applications
- `librewolf`: Privacy-focused Firefox fork
- `python`: Python interpreter
- `en-freebsd-doc`: FreeBSD documentation
- `nerd-fonts`: Patched fonts for terminals and editors

---

## Profile-Specific Lists
Each machine has its own package list in `besk/pkglist.txt` (or other profile names). This allows different configurations per system while sharing common packages.

## Missing from Binary Packages
Some software must be built from ports:
- **XLibre**: X.Org replacement (see main README)
- **Custom XMonad**: Built with specific configuration
- **Patched st terminal**: Custom patches applied
