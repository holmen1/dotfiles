# FreeBSD installation guide

Similar artixinstall, using dwm, less features

***Differences*** from Arch/Artix
- `bash` located in /usr/local/bin. So #!/bin/bash won't work.
- mount -t msdosfs
- GNU make vs BSD make — Use `gmake` for GNU Makefiles; FreeBSD's default `make` is BSD make with different syntax
- Header ordering in C network programming — On FreeBSD, include `<sys/types.h>` before `<sys/socket.h>` and other network headers to avoid compilation errors

## Stack

* Display server: XLibre (X11)
* Window manager: dwm
* Menus: dmenu
* Editor: Neovim
* Terminal: st
* File manager: lf
* Dotfile linking: stow
* Keymaps: setxkbmap / XKB

## Pre-installation

FreeBSD installation files are available at the [FreeBSD](https://www.freebsd.org/where/)
download page


```bash
dd if=FreeBSD-{rel-latest}-RELEASE-amd64-memstick.img of=/dev/da0 bs=1M conv=sync
```
`-memstick.img` contains all of the files needed to install FreeBSD, its source, and the Ports

## Guided installation

Once the system boots from the installation media, 
a text-based installation program named `bsdinstall` starts, chose defaults, use ethernet

otherwise my choice, see the [archived bsdinstall log](archive/bsdinstall_log.txt).

## Post install

### Network

Verify that networking works before installing packages or cloning the
repository. `bsdinstall` may already have configured the interface; inspect
the current settings before changing them:

```sh
ifconfig -a
cat /etc/rc.conf
netstat -rn
cat /etc/resolv.conf
```

Add one `network` block per Wi-Fi network to `/etc/wpa_supplicant.conf`
(create the file if needed):

```conf
ctrl_interface=/var/run/wpa_supplicant
eapol_version=1
ap_scan=1
fast_reauth=1

network={
    ssid="first-network-name"
    psk="first-network-password"
    priority=20
}

network={
    ssid="second-network-name"
    psk="second-network-password"
    priority=10
}
```
When multiple saved networks are available, wpa_supplicant prefers the one
with the higher `priority` value.


Obtain the network device interface (iwm0)
```bash
sysctl net.wlan.devices
```
```bash
sysrc wlans_iwm0="wlan0"
sysrc ifconfig_wlan0="WPA DHCP"
```

```bash
sysrc create_args_wlan0="country SE regdomain etsi2"
```
```bash
service netif restart
```

Check connectivity with `ping -c 3 1.1.1.1` and
`ping -c 3 freebsd.org`. For Wi-Fi setup, interface discovery, DHCP/DNS
behavior, and troubleshooting, see the [networking guide](networking.md)


### Prepare the account and hostname

### Add user

Add user to groups: wheel video (one at a time)


Install small set of useful tools:
```sh
pkg install git bash sudo lsblk
```

Set Bash as your login shell:
```sh
chsh -s /usr/local/bin/bash
```
Log out and back in for the new login shell to take effect.


### Sudo without password

Edit sudoers using `EDITOR=vim visudo` and enable:

```conf
%wheel ALL=(ALL:ALL) NOPASSWD: ALL
```
If `/usr/local/etc/sudoers.d/00_user` contains a user-specific rule, check
whether it overrides the wheel policy. Verify group membership with `groups`.

### Hostname

```sh
sysrc hostname="besk"
service hostname restart
hostname -s
```

### X

User must be a member of the video group to run a graphical environment:
```sh
pw groupmod video -m username
```

You can check that X starts before cloning and linking the full configuration:
```sh
sudo pkg install xorg xterm drm-kmod xf86-video-intel
sysrc kld_list+=i915kms
printf 'exec xterm\n' > ~/.xinitrc
startx -- -keeptty
```


### Run the BSD setup

Clone the repository over HTTPS so setup does not depend on an SSH key:

```sh
mkdir -p ~/repos
cd ~/repos
git clone https://github.com/holmen1/dotfiles.git
```

If your public key is already registered with GitHub, you can use
`git@github.com:holmen1/dotfiles.git` instead.

Then start the interactive setup:

```sh
cd ~/repos/dotfiles
./config/common/.scripts/configure-build-install-link.sh
```

Start X with the linked configuration and follow the [X11 guide](x11.md) for
troubleshooting:
```sh
startx -- -keeptty
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


---


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

---

### Missing from Binary Packages
Some custom software is built from source in this repository rather than
installed as a binary package:
- **Custom dwm**: Built with the repository's configuration
- **Patched st terminal**: Custom patches applied


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

## SSH server (remote login)

```sh
sudo sysrc sshd_enable=YES
sudo service sshd start
sudo service sshd status
```

## Further references

- Source builds: [build overview](../install/build/README.md) and [build guide](build.md).
- Shell one-liners: [shell guide](shell.md).
- X11 session startup and debugging: [X11 guide](x11.md) and [XLibre](../install/build/xlibre/README.md).
