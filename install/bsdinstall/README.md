# bsdinstall
## Pre-installation

* Collect Network Information

## Guided installation
Install FreeBSD using the text-based installation program named bsdinstall

Add user to groups: wheel video

Wifi: ETSI

otherwise my choice, see [user_configuration_x1.json](../archinstall/log/user_configuration_x1.json)  


## Post-installation
### Shell, also need adding hostname if used in scripts
```bash
chsh -s /usr/local/bin/bash
sysrc hostname="besk"
service hostname restart
```

### Wireless Configuration

Obtain the network device interface by using
```bash
sysctl net.wlan.devices
````

```bash
ifconfig wlan0 create wlandev iwm0
````

To make the change persist across reboots execute the following command:
```bash
sysrc wlans_iwm0="wlan0"
```

To list the wireless networks execute the following command:
```bash
ifconfig wlan0 up list scan
````

Once the scanning of the wireless networks has been carried out, a network has been chosen and have the password (PSK), that information will be added to the file ```/etc/wpa_supplicant```

```bash
network={
        scan_ssid=1 
        ssid="FreeBSD" 
        psk="12345678" 
}
```

To use a dynamic address it will be necessary to execute the following command:
```bash
sysrc ifconfig_wlan0="WPA DHCP"
````

Then restart the network executing the following command:
```bash
service netif restart
```

### Clone dotfiles
```bash
mkdir repos
git clone git@github.com:holmen1/dotfiles.git
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

### Install binaries
pre-built binaries from the [build factory](../build/):
```
cp /mnt/usb/st-0.9.2 /opt/st
cp /mnt/usb/xmonad-v0.18.0 /opt/xmonad/
```
and link to ```/usr/local/bin```


### Enable System Monitoring (Battery & WiFi)
```crontab```
```bash
DISPLAY=:0
*/2 * * * * /home/holmen1/repos/dotfiles/scripts/battery-monitor.sh >/dev/null 2>&1; /home/holmen1/repos/dotfiles/scripts/wifi-monitor.sh >/dev/null 2>&1
```

### Setting Brightness and Volume Keybindings with xbindkeys

1. First, install backlight
2. Create a config file:
   ```bash
   touch ~/.xbindkeysrc
   ```
3. Add brightness controls to ~/.xbindkeysrc:
   ```
   # Decrease brightness
   "backlight decr 10"
     F5

   # Increase brightness
   "backlight incr 10"
     F6
   ```
4. Add xbindkeys to your .xinitrc:
   ```bash
   # Launch key binding daemon
   xbindkeys &
   ```



### Sanity check
Run the sanity check script to verify your installation:
```bash
~/repos/dotfiles/install/bsdinstall/sanity_check.sh
```

This script verifies:
- Essential commands (git, ssh, xmonad, xterm)
- XMonad custom binary and configuration
- Screen locking (i3lock) functionality  
- Power management (wheel group, passwordless shutdown)
- System monitoring services
- Key bindings and input controls
- Screenshot functionality (scrot, Downloads directory)
- Dotfile symlinks
- Git and SSH configuration
- Repository structure


### Install Haskell
If ghcup has issues on FreeBSD 15, use native packages:
```bash
sudo pkg update
sudo pkg install ghc cabal-install haskell-language-server
```

Verify:
```bash
ghc --version
cabal --version
haskell-language-server --version
```

Add to your shell config:
```bash
export PATH="$HOME/.cabal/bin:$PATH"
```


### Building from Ports (For Custom/Latest Builds)

#### When to Use Ports vs Packages
Use `pkg` for most software installations. Build from ports when you need:
- Software not available as binary packages (e.g., xlibre)
- Custom compilation options or patches
- Latest development versions

#### Setting Up the Ports Tree
```bash
# Initial clone (if /usr/ports doesn't exist)
sudo git clone --depth 1 https://git.freebsd.org/ports.git /usr/ports

# Update existing ports tree
sudo git -C /usr/ports pull
```

#### Building XLibre (X.Org Replacement)
XLibre is a fork of X.Org that must be built from ports. This replaces the standard X11 implementation.

**Important:** Remove conflicting X.Org packages before installing xlibre:
```bash
# Check for conflicts
pkg info | grep -E 'xorg-server|xf86-'

# Remove X.Org packages if present
sudo pkg delete -f xorg-server xf86-input-libinput xf86-video-intel
```

**Build and install xlibre:**
```bash
# Install dependencies from binary packages (faster)
cd /usr/ports/x11/xlibre-minimal
make all-depends-list | cut -d/ -f5- | xargs sudo pkg install -y

# Build xlibre server
sudo make install clean

# Install display driver (required for graphics to work)
cd /usr/ports/x11-drivers/xlibre-xf86-video-intel  # or xf86-video-vesa for generic
sudo make install clean
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

**Distinguish between pkg-installed and port-built packages:**
```bash
# List only packages installed from official repositories
pkg query -e '%a = 0' '%R %o' | awk '$1 == "FreeBSD-ports" {print $2}' > pkg-installed.txt

# List only packages built from ports (local builds)
pkg query -e '%a = 0' '%R %o' | awk '$1 != "FreeBSD-ports" && $1 != "FreeBSD-base"  {print $2}' > port-built.txt
```

**Query field meanings:**
- `%a` - Automatic flag (0 = manually installed, 1 = dependency)
- `%o` - Origin (e.g., `x11/xlibre-minimal`)
- `%R` - Repository name (e.g., `FreeBSD`, empty for local builds)
- `%n` - Package name only

**Note:** Packages built from ports typically have `%R` set to empty or "FreeBSD-ports" depending on your configuration. Use `pkg info -o packagename` to verify the origin of specific packages.


# LESSONS LEARNED

## Differences from Arch notes

- No pacman / yay / AUR on FreeBSD — use `pkg` and `ports`.
- No systemd — use rc(8) and rc.conf
- mount -t msdosfs
- GNU make vs BSD make — Use `gmake` for GNU Makefiles; FreeBSD's default `make` is BSD make with different syntax
- Header ordering in C network programming — On FreeBSD, include `<sys/types.h>` before `<sys/socket.h>` and other network headers to avoid compilation errors






