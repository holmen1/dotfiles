# artixinstall

Port of archinstall to Artix Linux (OpenRC)

## Pre-installation

Boot the latest Artix Linux ISO (OpenRC variant).

### Keyboard layout
```
loadkeys sv-latin1
```


## Installation

### Identify the target disk

```
fdisk -l
```
Look for your NVMe drive, typically `/dev/nvme0n1`. Confirm size matches expectations before proceeding.

### Partition with fdisk

Target layout (UEFI, GPT):

| Partition | Mount | Size | Type |
|-----------|-------|------|------|
| nvme0n1p1 | /boot/efi | 1G | EFI System |
| nvme0n1p2 | [swap] | 8G | Linux swap |
| nvme0n1p3 | /home | rest | Linux filesystem |

```
fdisk /dev/nvme0n1
```

Inside the interactive prompt, follow this sequence:

```
Command (m for help): g
Created a new GPT disklabel.

Command (m for help): n
Partition number (1-128, default 1): <Enter>
First sector: <Enter>
Last sector: +1G

Command (m for help): n
Partition number (2-128, default 2): <Enter>
First sector: <Enter>
Last sector: +8G

Command (m for help): n
Partition number (3-128, default 3): <Enter>
First sector: <Enter>
Last sector: <Enter>    (uses all remaining space)

Command (m for help): p
(verify the table looks correct)

Command (m for help): w
```

Expected result:
```
/dev/nvme0n1p1   2048    2099199    1G  Linux filesystem
/dev/nvme0n1p2   2099200 18874367   8G  Linux filesystem
/dev/nvme0n1p3  18874368 <end>      <rest>  Linux filesystem
```

> **Note:** After writing, the kernel may not see all partitions immediately ("device busy"). Verify with `lsblk`. If pX is missing, reboot the ISO — the table is correctly on disk and all 3 will appear after reboot.

### Format partitions

Labels (`-L` / `-n`) make the partitions identifiable by name instead of device path:

```
mkfs.fat -F 32 /dev/nvme0n1p1            # EFI system partition
fatlabel /dev/nvme0n1p1 ESP
mkswap -L SWAP /dev/nvme0n1p2            # swap
mkfs.ext4 -L ROOT /dev/nvme0n1p3         # root
```

### Mount

```
swapon /dev/disk/by-label/SWAP
mount /dev/disk/by-label/ROOT /mnt
mkdir -p /mnt/boot/efi
mount /dev/disk/by-label/ESP /mnt/boot/efi
```

Verify:
```
lsblk
free -h    # swap should appear
```

### Network

> **Use ethernet during installation.** WiFi can be configured properly post-installation. Attempting to set up WiFi at this stage is a known failure point.

Plug in ethernet and verify connectivity:
```
ping -c 3 artixlinux.org
```

### Update the system clock
Activate the NTP daemon to synchronize the computer's real-time clock:

```bash
rc-service ntpd start
```

### Install base system

```
basestrap /mnt base base-devel openrc elogind-openrc
```

- `openrc` — init system
- `elogind-openrc` — session/seat management (replaces systemd-logind)

If you encounter errors, you can use the -i flag of basestrap ('interactive'). Example:

```bash
basestrap -i /mnt base
```

### Install a kernel
Artix provides three kernels: linux, linux-lts and linux-zen. It is very recommended to install linux-firmware too.
You can try not installing it, but some devices such as network cards may not work.

```bash
basestrap /mnt linux linux-firmware
```

and, nice to have
```
basestrap /mnt sudo git vim openssh
```

### Generate fstab

> **Note:** `basestrap` must have run first — it creates `/mnt/etc/`. If you get "no such file or directory", run `mkdir -p /mnt/etc` and retry.

```
fstabgen -U /mnt >> /mnt/etc/fstab
```

Inspect the result and make sure all partitions are listed (root, efi, swap, [home]):
```
cat /mnt/etc/fstab
```

```
# Static information about the filesystems.
# See fstab(5) for details.

# <file system> <dir> <type> <options> <dump> <pass>
# /dev/nvme0n1p3 LABEL=ROOT
UUID=740223d7-6729-46a8-bf83-b68329a3e33b	/         	ext4      	rw,relatime	0 1

# /dev/nvme0n1p1 LABEL=ESP
UUID=AF1B-0335      	/boot/efi 	vfat      	rw,relatime,fmask=0022,dmask=0022,codepage=437,iocharset=ascii,shortname=mixed,utf8,errors=remount-ro	0 2

# /dev/nvme0n1p2 LABEL=SWAP
UUID=3dbe3090-4cf3-49be-9a7c-2cff14d9a484	none      	swap      	defaults  	0 0
```

### Chroot into the new system

```
artix-chroot /mnt
```

### Configure the system clock
Set the time zone:
```
ln -sf /usr/share/zoneinfo/Europe/Stockholm /etc/localtime
```
Run hwclock to generate /etc/adjtime:
```
hwclock --systohc
```

### Locale

Edit `/etc/locale.gen` and uncomment `en_US.UTF-8 UTF-8`, then:
```
locale-gen
echo "LANG=en_US.UTF-8" > /etc/locale.conf
```

### Console keymap

```
echo 'keymap="sv-latin1"' > /etc/conf.d/keymaps
```

### Bootloader (GRUB)

If `pacman` fails with connection errors (port 443), fix DNS and refresh mirrors first:
```
echo "nameserver 8.8.8.8" > /etc/resolv.conf
pacman -Sy
```

```
pacman -S grub efibootmgr
grub-install --target=x86_64-efi --efi-directory=/boot/efi --bootloader-id=grub
grub-mkconfig -o /boot/grub/grub.cfg
````

### Add users

- Root
```
passwd
```

- User
```
useradd -m -G wheel,video holmen1
passwd holmen1
```
video needed to run `brightnessctl`

To add when already in group
```
sudo usermod -aG input $USER
```
or `audio`

Allow wheel group to use sudo [without password] — run `EDITOR=vim visudo` and uncomment:
```
%wheel ALL=(ALL:ALL) [NOPASSWD:] ALL
```

### Network configuration

```
echo gadsden > /etc/hostname
echo 'hostname="gadsden"' > /etc/conf.d/hostname
```

Configure `/etc/hosts`:
```
127.0.0.1   localhost
::1 localhost
127.0.1.1   gadsden.localdomain gadsden
```

Install `iwd`, D-Bus, and network support packages, then enable them at boot:
```
pacman -S iwd-openrc dbus-openrc dhcpcd dhcpcd-openrc openresolv
rc-update add dbus default
rc-update add iwd default
rc-update add dhcpcd default
```

`iwd` handles WiFi association and IP assignment (DHCP). `dhcpcd` handles wired ethernet. `openresolv` provides `resolvconf` for automatic DNS updates.

Create `/etc/iwd/main.conf` to enable IP assignment and DNS integration:
```
mkdir -p /etc/iwd
```
```ini
# /etc/iwd/main.conf
[General]
EnableNetworkConfiguration=true

[Network]
NameResolvingService=resolvconf
```

Without `EnableNetworkConfiguration=true`, `iwd` associates to WiFi but never assigns an IP address — the most common post-install networking failure.

Prevent `dhcpcd` from interfering with `wlan0` (iwd manages WiFi DHCP itself):
```
echo "denyinterfaces wlan*" >> /etc/dhcpcd.conf
```

### Exit chroot and reboot

```
exit
umount -R /mnt
reboot
```

Remove the ISO/USB when the machine powers off.

---

## Post-installation

> You need a working network connection before running the post-install script. Keep ethernet connected until WiFi is configured and verified.

Log in as `holmen1`.

### Connect to WiFi

`iwd` and `dbus` should be running (enabled in chroot). Use `iwctl` to connect:

```
iwctl
[iwd]# device list
[iwd]# station wlan0 scan
[iwd]# station wlan0 get-networks
[iwd]# station wlan0 connect YOUR_SSID
[iwd]# exit
```

Verify connectivity:
```
ping -c 3 artixlinux.org
```

If `iwd` is not running, start it first:
```
sudo rc-service dbus start
sudo rc-service iwd start
```

Verify:
```
ip addr show wlan0        # should show inet address
ping -c 3 1.1.1.1         # tests IP routing
ping -c 3 artixlinux.org  # tests DNS
```

### Clone dotfiles

```
mkdir repos
cd repos
git clone https://github.com/holmen1/dotfiles.git
```

#### Switch to SSH (reusing existing key)

Copy your existing private key to the new machine, then:
```
chmod 600 ~/.ssh/id_ed25519
```

Verify the key works with GitHub:
```
ssh -T git@github.com
```

Switch the remote from HTTPS to SSH:
```
cd ~/repos/dotfiles
git remote set-url origin git@github.com:holmen1/dotfiles.git
```

Verify:
```
git remote -v
```
```
origin  git@github.com:holmen1/dotfiles.git (fetch)
origin  git@github.com:holmen1/dotfiles.git (push)
```

### Install

Some need to built from source

#### 0. Synchronizes the repository databases and updates the system's packages

```bash
pacman -Syu
```

#### 1. Install X server (Xlibre)

`xlibre-xserver` and `xlibre-input-libinput` are in the Artix **world** repository — install directly with pacman, no AUR or yay needed:

```
sudo pacman -S xlibre-xserver xlibre-input-libinput
```

#### 2. Install X stuff

[x_pkg.txt](install/artixinstall/packages/x_pkg.txt)
```bash
./install/artixinstall/scripts/install-pacman.sh install/artixinstall/packages/x_pkg.txt
```

Verify by creating a minimal `.xinitrc`
```bash
echo "exec xterm" > ~/.xinitrc
```

then
```bash
startx
```
a terminal should open, may need be pointed to to activate

Monitor
`~/.local/share/xorg/Xorg.0.log`
for errors



#### 3. Build or install Haskell Compiler (GHC)

For systems requiring Haskell development or custom xmonad builds:

- [GHC build scripts](../build/ghc/) - Install Haskell compiler from source
- Provides `ghc`, `runhaskell`, and Cabal library for building Haskell packages

#### 4. XMonad Window Manager

Building custom xmonad without cabal-install dependency:

- [XMonad build scripts](../build/xmonad/) - Build xmonad and xmonad-contrib from source
- Uses `runhaskell Setup.lhs` with GHC's built-in Cabal library
- Produces standalone binaries without cabal-install dependency

#### 5. Run install script

Press enter to skip steps, `y` as shown below.
This will:
- Install pkglist
- Install `xmonad`
- Build and install `st`
- Link dotfiles
- Enable services (openrc)
- Run sanity check
```
cd ~/repos/dotfiles/install/artixinstall
./configure_build_install_link.sh
```
```bash
/home/holmen1/repos/dotfiles/install/artixinstall/packages/gadsden/pkglist.txt
Install pkglist? [y]
Install xmonad? [y]
Build st? [y]
Install st? [y]
Link dotfiles? [y]
Enable services? [y]
Run tests? [Y]
```

### Package management

Artix uses `pacman` backed by Artix and Arch repos. AUR packages via `yay`.

OpenRC equivalents for common packages:
- `networkmanager-openrc`
- `iwd-openrc`
- `dbus-openrc`

See [packages/gadsden/](packages/gadsden/) for the full package lists.

### Export installed packages

```
~/repos/dotfiles/install/artixinstall/scripts/export-pacman.sh \
  ~/repos/dotfiles/install/artixinstall/packages/<hostname>
```


---

## REPAIR

Boot from ISO, then remount and chroot:

```
fdisk -l                            # confirm partition layout
mount /dev/nvme0n1p3 /mnt
mkdir -p /mnt/boot/efi
mount /dev/nvme0n1p1 /mnt/boot/efi
mkdir -p /mnt/home
mount /dev/nvme0n1p4 /mnt/home
swapon /dev/nvme0n1p2
artix-chroot /mnt
```

---

See [LESSONS_LEARNED.md](LESSONS_LEARNED.md) for lessons learned.
