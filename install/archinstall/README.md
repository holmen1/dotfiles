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

### SSH
Reuse key already in Github

```
cp /mnt/usb/id_ed25519* ~/.ssh/
```
Start the ssh-agent in the background
```
chmod 600 ~/.ssh/id_ed25519
eval "$(ssh-agent -s)"
ssh-add ~/.ssh/id_ed25519
```

### Configure Git
```
git config --global user.name "$git_username"
git config --global user.email "$git_email"
```

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


# LESSONS LEARNED

## Update linux-firmware
### Manual Intervention Required (June 2025)

**Important:**  
Starting with `linux-firmware >= 20250613.12fe085f-5`, the package was split and the NVIDIA firmware layout changed. Upgrading from an earlier version may cause errors like:

```
linux-firmware-nvidia: /usr/lib/firmware/nvidia/ad103 exists in filesystem
...
```

#### Solution

1. **Download new firmware packages before removing the old one (to avoid losing WiFi):**
    ```
    sudo pacman -Sw linux-firmware
    ```

2. **Remove the old package (this does NOT delete firmware files from disk):**
    ```
    sudo pacman -Rdd linux-firmware
    ```

3. **Upgrade and reinstall:**
    ```
    sudo pacman -Syu linux-firmware
    ```

> **Note:** Removing `linux-firmware` with `-Rdd` only removes the package from the database. The actual firmware files remain on disk, so your WiFi and other hardware should keep working during the upgrade.

**References:**  
- [Arch Linux News: linux-firmware upgrade requires manual intervention](https://archlinux.org/news/linux-firmware-20250613-12fe085f-5-upgrade-requires-manual-intervention/)

### If You Lose WiFi: Update with Archinstall ISO

If you lose WiFi connectivity during a `linux-firmware` upgrade and cannot restore it from your installed system, you can use the Arch Linux ISO (live environment) to repair your system:

1. **Boot from the latest Arch Linux ISO (USB/DVD).**

2. **Connect to the internet using a wired (Ethernet) connection if possible.**  
   If WiFi is unavailable, Ethernet is the most reliable option.

3. **Mount your root partition and (if needed) other subvolumes:**
    ```
    # mount -o subvol=@ /dev/<root-partition> /mnt
    # mount -o subvol=@home /dev/<root-partition> /mnt/home
    ```
    *(Replace `<root-partition>` with your actual device, e.g., `nvme0n1p2`.)*

4. **Mount other necessary filesystems:**
    ```
    # mount /dev/<efi-partition> /mnt/boot
    # mount --bind /dev /mnt/dev
    # mount --bind /proc /mnt/proc
    # mount --bind /sys /mnt/sys
    ```

5. **Chroot into your installed system:**
    ```
    # arch-chroot /mnt
    ```

6. **Update and reinstall firmware packages:**
    ```
    # pacman -Syu linux-firmware
    ```
    *(Add any other firmware packages you need, e.g., `linux-firmware-nvidia-gsp`.)*

7. **Exit chroot and reboot:**
    ```
    # exit
    # reboot
    ```

Your system should now have the latest firmware and, if supported, WiFi should work again.

---

**Tip:**  
If you need to download packages on another machine, you can copy them to `/mnt/var/cache/pacman/pkg/` before installing with `pacman -U`.

---

**References:**  
- [Arch Wiki: Chroot](https://wiki.archlinux.org/title/Chroot)
- [Arch Wiki: General Troubleshooting](https://wiki.archlinux.org/title/General_troubleshooting)



## Gotchas
```
$ systemctl status NetworkManager
```









