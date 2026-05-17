# archinstall
## Pre-installation
In the latest Arch Linux ISO

* Set the console keyboard
```
loadkeys sv-latin1
```
* Connect to the internet
Find device name
```
root@archiso ~ # ip addr show
```
2: wlan0: ...
```
iwctl station wlan0 scan
iwctl station wlan0 get-networks
```

```
root@archiso ~ # iwctl --passphrase <passphrase> station wlan0 connect <NetworkName>
```
## Guided installation
Archinstall ships with a pre-programmed Guided Installer guiding you through the mandatory steps as well as some optional configurations that can be done.   
To start the installer, run
```
root@archiso ~ # archinstall
```

Set 
bootloader: grub   (fallback: systemd-boot)
profile:    Minimal  
Filesystem: btrfs  
packages: git, openssh, vi


otherwise my choice, see [user_configuration_x1.json](../archinstall/log/user_configuration_x1.json)  


## Post-installation

### XLibre
X11 display server. Striving to improve the existing code base while maintaining backward compatibility to make X11 a viable choice for the future  

Build and install from AUR, see [README](../build/xlibre/README.md)

### Clone dotfiles
```
mkdir repos
git clone git@github.com:holmen1/dotfiles.git
```

### Install packages

First install yay
```
sudo pacman -S --needed git base-devel && git clone https://aur.archlinux.org/yay.git && cd yay && makepkg -si
```
then
```
./scripts/yay_install.sh ~/[path]/pkglist.txt
```
dont forget ```foreignpkglist.txt``` if any

See [packages](packages/README.md) for more


### Install binaries
pre-built binaries from the [build factory](../build/):
```
cp /mnt/usb/st-0.9.2 /opt/st
cp /mnt/usb/xmonad-v0.18.0 /opt/xmonad/
```
and link to ```/usr/local/bin```

### Link dotfiles
```
link_x_config.sh
```

### Enable System Monitoring (Battery & WiFi)

```bash
systemctl --user daemon-reload
systemctl --user enable system-monitor.timer
systemctl --user start system-monitor.timer
```
### Setting Brightness and Volume Keybindings with xbindkeys

1. First, install xbindkeys
2. Create a config file:
   ```bash
   touch ~/.xbindkeysrc
   ```
3. Add brightness controls to ~/.xbindkeysrc:
   ```
   # Decrease brightness
   "brightnessctl set 10%-"
     XF86MonBrightnessDown

   # Increase brightness
   "brightnessctl set +10%"
     XF86MonBrightnessUp

   # Simple ALSA Volume Controls
   "amixer -q set Master 5%+ unmute"
    XF86AudioRaiseVolume

   "amixer -q set Master 5%- unmute"
    XF86AudioLowerVolume

   "amixer -q set Master toggle"
    XF86AudioMute
   ```
4. Add xbindkeys to your .xinitrc:
   ```bash
   # Launch key binding daemon
   xbindkeys &
   ```



### Sanity check
Run the sanity check script to verify your installation:
```bash
~/repos/dotfiles/install/archinstall/sanity_check.sh
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


* Install Haskell
```
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

* Build XMonad





## REPAIR
On a Btrfs root file system with subvolumes, you have to make sure that all subvolumes are properly mounted
as specified in fstab before entering chroot

```
root@archiso ~ # fdisk -l
/dev/nvme0n1p1    2048    2099199   2097152     1G EFI System
/dev/nvme0n1p2 2099200 1000213167 998113968 475.9G Linux root (x86-64)
```

```
# mount -o subvol=@ /dev/nvme0np2 /mnt
# mount -o subvol=@home /dev/nvme0np2 /mnt/home
```

```
# lsblk 
```

```
# arch-chroot /mnt
```

chroot to user
```
[root@archiso /]# su - holmen1
```


```
[root@archiso /]# exit
# cd /
# umount --recursive /pa
```





### Export installed packages
Keeping a list of all explicitly installed packages can be useful to backup a system or quicken the installation of a new one
```
$ .repos/dotfiles/scripts/export_packages.sh ~/repos/linuxinstall/arch/packages/
```
creates files pkglist.txt and foreignpkglist.txt in the packages directory


See [LESSONS_LEARNED.md](LESSONS_LEARNED.md) for lessons learned.

## Gotchas
```
$ systemctl status NetworkManager
```









