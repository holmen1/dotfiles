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
When `pkg` doesn't have what you need (e.g., patched XMonad, Xlibre), build from ports. Use Git for the ports tree (faster than the old portsnap):

```bash
# Clone/update ports tree
sudo git clone https://git.freebsd.org/ports.git /usr/ports
cd /usr/ports && sudo git pull

# Build XMonad (example)
cd /usr/ports/x11-wm/xmonad && sudo make install clean

# Build Xlibre (if available in ports)
cd /usr/ports/x11/xorg && sudo make install clean  # Adjust path as needed
```


### Export installed packages
Keeping a list of all explicitly installed packages can be useful to backup a system or quicken the installation of a new one

Exports ONLY manually installed packages
```bash
pkg query -e '%a = 0' %n > pkglist.txt
```

If you want to include foreign packages (built from ports), add a separate export:
```bash
pkg query -e '%a = 0 && %o != ports' %n > foreignpkglist.txt
````
(though this is rare on FreeBSD)


# LESSONS LEARNED

## Differences from Arch notes

- No pacman / yay / AUR on FreeBSD — use `pkg` and `ports`.
- No systemd — use rc(8) and rc.conf
- mount -t msdosfs
- GNU make vs BSD make — Use `gmake` for GNU Makefiles; FreeBSD's default `make` is BSD make with different syntax
- Header ordering in C network programming — On FreeBSD, include `<sys/types.h>` before `<sys/socket.h>` and other network headers to avoid compilation errors






