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
root@archiso ~ # iwctl --passphrase <passphrase> station wlan0 connect NetworkName
```
## Guided installation
Archinstall ships with a pre-programmed Guided Installer guiding you through the mandatory steps as well as some optional configurations that can be done.   
To start the installer, run
```
root@archiso ~ # archinstall
```

Note, set 
"bootloader": "Systemd-boot" 
"profile":    "Xorg"

otherwise my choice, see [install.log](../log/install.log)  


## Post-installation

* Install Haskell
```
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

* Build XMonad



* install packages
```
./scripts/pacman_install.sh ~/repos/dotfiles/packages/xmonad/pkglist.txt
./scripts/yay_install.sh ~/repos/dotfiles/packages/xmonad/foreignpkglist.txt
```
* link configuration dotfiles to ~/.config

```
./scripts/link_config.sh ~/repos/archinstall
```


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


## Packages

```
sudo pacman -S <package>
sudo pacman -Rs <package>
```
or
```
yay -S <package>
yay -Rs <package>
```


### Export installed packages
Keeping a list of all explicitly installed packages can be useful to backup a system or quicken the installation of a new one
```
$ .repos/dotfiles/scripts/export_packages.sh ~/repos/linuxinstall/arch/packages/
```
creates files pkglist.txt and foreignpkglist.txt in the packages directory


# LESSONS LEARNED

## Gotchas
```
$ systemctl status NetworkManager
```









