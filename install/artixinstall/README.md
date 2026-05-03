# artixinstall
## Pre-installation
In the latest Artix Linux ISO (OpenRC)

* Set the console keyboard
```
loadkeys sv-latin1
```
* Connect to the internet (live ISO uses ConnMan)
```
connmanctl
connmanctl> enable wifi
connmanctl> scan wifi
connmanctl> services
connmanctl> connect wifi_<id>
connmanctl> quit
```

## Installation
Artix uses a manual installation process (no guided installer like archinstall).

### Partition & format
```
cfdisk /dev/nvme0n1
mkfs.fat -F 32 /dev/nvme0n1p1    # EFI
mkfs.btrfs /dev/nvme0n1p2         # root
```

### Mount
```
mount /dev/nvme0n1p2 /mnt
mkdir -p /mnt/boot/efi
mount /dev/nvme0n1p1 /mnt/boot/efi
```

### Install base system
```
basestrap /mnt base base-devel openrc elogind-openrc linux linux-firmware git vi
```

### Generate fstab
```
fstabgen -U /mnt >> /mnt/etc/fstab
```

### Chroot
```
artix-chroot /mnt
```

### Inside chroot: grub, hostname, user
```
pacman -S grub efibootmgr
grub-install --target=x86_64-efi --efi-directory=/boot/efi --bootloader-id=grub
grub-mkconfig -o /boot/grub/grub.cfg

echo gadsden > /etc/hostname
echo 'hostname="gadsden"' >> /etc/conf.d/hostname

useradd -m -G wheel holmen1
passwd holmen1
```

## Post-installation

### Clone dotfiles
```
mkdir repos
git clone git@github.com:holmen1/dotfiles.git
```

### Run install script
```
cd ~/repos/dotfiles/install/artixinstall
sh configure_build_install_link.sh
```

This will prompt to:
1. Configure git
2. Generate SSH key
3. Install yay (AUR helper)
4. Install pkglist / foreignpkglist
5. Link dotfiles
6. Enable services (openrc)
7. Run sanity check

### Package management
Artix uses `pacman` + artix repos. AUR packages work via `yay`.

Note: Replace systemd packages with openrc equivalents, e.g.:
- `networkmanager` → `networkmanager` + `networkmanager-openrc`
- `iwd` → `iwd` + `iwd-openrc`

See [packages/](packages/) for package lists.

### Enable services (openrc)
```
rc-update add iwd default
rc-update add dbus default
```

### Link dotfiles
```
~/repos/dotfiles/scripts/link_config.sh ~/repos/dotfiles/install/artixinstall/links/gadsden/links.config
```

### Export installed packages
```
~/repos/dotfiles/install/artixinstall/scripts/export-pacman.sh ~/repos/dotfiles/install/artixinstall/packages/gadsden/
```

### Sanity check
```
~/repos/dotfiles/install/artixinstall/tests/gadsden/sanity_check.sh
```

## REPAIR
```
mount -o subvol=@ /dev/sdXY /mnt
mount -o subvol=@home /dev/sdXY /mnt/home
artix-chroot /mnt
su - holmen1
```

See [LESSONS_LEARNED.md](LESSONS_LEARNED.md) for lessons learned.
