# FreeBSD post-install guide

Continue from the [FreeBSD installation guide](bsd-install-guide.md).

## Post-installation

Log in as root
### sudo

- Install sudo
- Enable passwordless for `wheel`

### Window Manager

```bash
sudo pkg install xlibre [xlibre-xf86-video-intel]
echo "exec xterm" > ~/.xinitrc
startx -- -keeptty
```
[TROUBLESHOOTING XLIBRE](../install/build/xlibre/README.md)

### Initial utilities

- Install: `git`, `bash`, `curl`


### Shell, also need adding hostname if used in scripts

```bash
chsh -s /usr/local/bin/bash
sysrc hostname="besk"
service hostname restart
```

### Clone dotfiles
```bash
mkdir repos
cd repos
git clone git@github.com:holmen1/dotfiles.git
```

#### Run install script

Press enter to skip steps, `y` as shown below.
This will:
- Install pkglist
- Build and install `st`
- Link dotfiles
- Link utils to `/usr/local/bin`
- Run sanity check
```
cd ~/repos/dotfiles/install/profiles/bsdinstall
./configure-build-install-link.sh
```

## Disaster recovery

1. **Reboot and enter single-user mode** from the boot menu.
2. At the prompt, remount filesystems:
	```
	mount -u /
	mount -a
	```
3. Check `/etc/rc.conf`
4. Check for recent changes to `/etc/sysctl.conf` or `/etc/ttys`
5. If you enabled xxx at the system level, disable it:
	```
	service xxx stop
	sysrc xxx_enable=NO
	```

### Logs
`/var/log/daemon.log` is the primary log for system services — wpa_supplicant, dhclient, and other rc.d daemons write here.

```bash
tail -f /var/log/daemon.log          # live view
```

To list the wireless networks execute the following command:
```bash
ifconfig wlan0 up list scan
```

Then restart the network executing the following command:
```bash
service netif restart
```

### Install packages

Restore from pkg list:
```bash
xargs sudo pkg install -y < pkglist.txt
```

Build from ports (htop example):
```bash
cd /usr/ports/htop
sudo make [install/deinstall/build/clean]
```

### Autologin

Edit
`/etc/gettytab`
```bash
# Autologin - automatically log in as username
#

Al|Autologin:\
	:ht:np:sp#115200:al=username:
```
and
`/etc/ttys`
```bash
`---`ttyv0	"/usr/libexec/getty Pc"		xterm	onifexists secure
`+++`ttyv0	"/usr/libexec/getty Al"		xterm	onifexists secure
```

### Setting Brightness and Volume Keybindings with xbindkeys

Enable ACPI in
`/boot/loader.conf`
```bash
[...]
acpi_video_load="YES"
```


### Sanity check
Run the sanity check script to verify your installation:
```bash
~/repos/dotfiles/install/profiles/bsdinstall/tests/$(hostname -s)/sanity_check.sh
```

This script verifies:
- Essential commands (git, ssh, dwm, xterm)
- Power management (wheel group, passwordless shutdown)
- System monitoring services
- Key bindings and input controls
- Dotfile symlinks
- Git and SSH configuration
- Repository structure

### Building from Ports (For Custom/Latest Builds)

#### When to Use Ports vs Packages
Use `pkg` for most software installations. Build from ports when you need:
- Software not available as binary packages
- Custom compilation options or patches
- Latest development versions

#### Setting Up the Ports Tree
```bash
# Initial clone (if /usr/ports doesn't exist)
sudo git clone --depth 1 https://git.freebsd.org/ports.git /usr/ports

# Update existing ports tree
sudo git -C /usr/ports pull
```

#### Resolving Package Conflicts
If you encounter registration conflicts during installation:
```bash
# Force package registration (use cautiously)
sudo make install clean FORCE_PKG_REGISTER=YES
```

**Warning:** Only use `FORCE_PKG_REGISTER` when you're certain the conflicting package has been properly removed and the conflict is a registration artifact.

#### Port Build Options
During port builds, you'll see configuration dialogs with checkboxes for features. Understanding these helps avoid build issues and unnecessary bloat.

**Common options and recommendations:**

| Option | Description | Recommendation |
|--------|-------------|----------------|
| `DOCS` | Install documentation | Disable to save space (~5-50MB per port) |
| `EXAMPLES` | Install example files | Disable unless learning the software |
| `NLS` | Native Language Support (translations) | Disable if you only use English (saves 10-50MB) |
| `X11` | X Window System support | Keep enabled if using a GUI |
| `WAYLAND` | Wayland support | Disable if using X11/xlibre only |
| `DEBUG` | Debug symbols | Disable unless developing (adds significant size) |
| `IPV6` | IPv6 protocol support | Keep enabled (modern standard) |
| `TEST` | Build and run tests | Disable (only for development) |

**Best practices for beginners:**
1. **First build:** Accept all defaults - they're tested and safe
2. **Rebuilding for optimization:** Disable `DOCS`, `EXAMPLES`, `NLS`, `DEBUG` to save ~20-30% space
3. **Critical ports (xlibre, gcc, llvm):** Use defaults - wrong options can break your system
4. **Desktop applications:** Verify `X11` is enabled

**Configuration commands:**
```bash
# Configure build options interactively
cd /usr/ports/category/portname
sudo make config

# Configure port and all its dependencies (careful - many dialogs)
sudo make config-recursive

# Reset to defaults if you made mistakes
sudo make rmconfig

# Show current configuration without dialog
make showconfig
```

**Time/space tradeoffs:**
- Disabling `DOCS`, `NLS`, `EXAMPLES`: Saves disk space, minimal impact on build time
- Disabling optional features (e.g., `WAYLAND` when using X11): Reduces dependencies, faster builds
- Enabling `DEBUG`: Significantly increases binary size (+50-200%) and build time (+10-30%)



### Export installed packages
Keeping a list of all explicitly installed packages can be useful to backup a system or quicken the installation of a new one.

**Export manually installed packages:**
```bash
pkg query -e '%a = 0' %o > pkglist.txt
```

**Inspect repository metadata for manually installed packages:**

Repository names vary by configuration, and local or missing repository
metadata does not reliably prove that a package was built from ports. Check
the results before using them to decide what to rebuild.

```bash
# Example: packages recorded as coming from FreeBSD-ports
pkg query -e '%a = 0' '%R %o' | awk '$1 == "FreeBSD-ports" {print $2}' > pkg-installed.txt

# Example: packages recorded with another or empty repository name
pkg query -e '%a = 0' '%R %o' | awk '$1 != "FreeBSD-ports" && $1 != "FreeBSD-base"  {print $2}' > port-built.txt
```

**Query field meanings:**
- `%a` - Automatic flag (0 = manually installed, 1 = dependency)
- `%o` - Origin (e.g., `x11/xlibre-minimal`)
- `%R` - Repository name (e.g., `FreeBSD`, empty for local builds)
- `%n` - Package name only


## Lessons learned

See [install/build/xlibre/README.md](../install/build/xlibre/README.md) for XLibre-specific lessons learned.

---

### Differences from Arch

- `bash` located in /usr/local/bin. So #!/bin/bash won't work.
- No pacman / yay / AUR on FreeBSD — use `pkg` and `ports`.
- No systemd — use rc(8) and rc.conf
- mount -t msdosfs
- GNU make vs BSD make — Use `gmake` for GNU Makefiles; FreeBSD's default `make` is BSD make with different syntax
- Header ordering in C network programming — On FreeBSD, include `<sys/types.h>` before `<sys/socket.h>` and other network headers to avoid compilation errors


## Package management

### Usage
```bash
# Update all packages
sudo pkg upgrade

# Install a new package
sudo pkg install <package-name>

# Search for packages
pkg search <keyword>

# Remove a package
sudo pkg delete <package-name>

# Remove unused dependencies after removing packages
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

### Package management philosophy
- **Use `pkg` for binary packages** - Fast, tested, recommended for most software
- **Use ports for custom builds** - When you need specific options or software unavailable as packages (see [Building from Ports](#building-from-ports-for-customlatest-builds))
- **Keep it minimal** - FreeBSD doesn't bloat by default; install only what you need

### Descriptions

#### Core System
- `FreeBSD-set-base`, `FreeBSD-set-devel`, `FreeBSD-set-minimal`: Essential FreeBSD base system
- `FreeBSD-kernel-generic`, `FreeBSD-kernel-generic-dbg`: Kernel and debugging symbols
- `FreeBSD-set-src`: System sources (for ports and kernel development)
- `FreeBSD-set-lib32`: 32-bit libraries (for compatibility)
- `bash`: Bourne Again Shell (set as user shell via `chsh`)
- `sudo`: Execute commands as root
- `ca_root_nss`: Root SSL certificates
- `pkg`: Package manager
- `portconfig`: Configure port build options

#### Hardware & Drivers
- `drm-kmod`: Direct Rendering Manager kernel modules (modern GPU support)
- `wifi-firmware-iwlwifi-kmod-7000`: Intel WiFi firmware (for Intel wireless cards)

#### Display Server & Graphics
- `xlibre-minimal`: XLibre server (X.Org fork, now available as binary package)
- `xlibre-xf86-video-intel`: Intel graphics driver for XLibre
- `xsetroot`: Set X root window properties
- `xterm`: Terminal emulator for X

#### Window Manager & Desktop Utilities
- `dmenu`: Dynamic menu for X
- `i3lock`: Screen locker
- `xbindkeys`: Keybinding daemon
- `xclip`: Clipboard integration for X
- `xcompmgr`: Simple X compositor (transparency, shadows)
- `feh`: Lightweight image viewer (wallpapers)
- `scrot`: Screenshot tool
- `dunst`: Lightweight notification daemon
- `setxkbmap`: Set X keyboard layout

#### Development Tools
- `git`: Version control
- `gcc`: GNU Compiler Collection
- `gdb`: GNU Debugger
- `gmake`: GNU Make (many projects require this instead of BSD make)
- `llvm`: LLVM compiler infrastructure
- `bison`: Parser generator (required by some build processes)
- `pkgconf`: Package config tool (pkg-config alternative)

#### Haskell Development (for XMonad)
- `ghc`: Glasgow Haskell Compiler
- `hs-cabal-install`: Haskell package manager
- `hs-haskell-language-server`: LSP server for Haskell

#### Editors & LSP
- `neovim`: Modern Vim fork
- `lua-language-server`: LSP for Lua (Neovim config)
- `stylua`: Lua formatter
- `tree-sitter-cli`: Parser generator for syntax highlighting

#### Utilities
- `fd`: Fast find alternative
- `ripgrep`: Fast grep alternative
- `tree`: Display directory structure
- `lf`: Terminal file manager
- `htop`: Interactive process viewer
- `fastfetch`: System information tool
- `wget`: Network downloader
- `ncurses`: Terminal UI library

#### Applications
- `librewolf`: Privacy-focused Firefox fork
- `python`: Python interpreter
- `en-freebsd-doc`: FreeBSD documentation
- `jetbrains-mono-nerd-font`: JetBrains Mono Nerd Font (individual package, much faster to upgrade than full nerd-fonts collection)

---

### Profile-Specific Lists
Each machine has its own package list in `besk/pkglist.txt` (or other profile names). This allows different configurations per system while sharing common packages.

### Missing from Binary Packages
Some custom software is built from source in this repository rather than
installed as a binary package:
- **Custom XMonad**: Built with specific configuration
- **Patched st terminal**: Custom patches applied

> **Note:** XLibre was previously ports-only but is now available as a binary package. Always check `pkg search <name>` before resorting to ports — the situation changes as the ports tree matures.

### Migrating a Port to a Binary Package
When a previously ports-only package becomes available via `pkg`:

1. **Verify it's available:**
   ```bash
   pkg search <name>
   ```
2. **Deinstall the port-built version:**
   ```bash
   sudo pkg delete -f <portname>
   sudo pkg autoremove
   ```
3. **Install the binary package:**
   ```bash
   sudo pkg install <name>
   ```
4. **Update tracking files:** Move the entry from `foreignpkglist.txt` to `pkglist.txt`.
