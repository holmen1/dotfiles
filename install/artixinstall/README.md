# artixinstall
## Pre-installation
In the latest Artix Linux ISO (OpenRC)

* Set the console keyboard
```
loadkeys sv-latin1
```
* Connect to the internet
```
ip addr show
```
```
iwctl station wlan0 scan
iwctl station wlan0 get-networks
iwctl --passphrase <passphrase> station wlan0 connect <NetworkName>
```

## Installation
Artix ships with a guided installer:
```
artixinstall
```
Set:
- bootloader: grub
- profile: Minimal
- filesystem: btrfs
- init: openrc
- packages: git, openssh, vi

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
rc-update add NetworkManager default
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
