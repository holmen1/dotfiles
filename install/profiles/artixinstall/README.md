# artixinstall

Artix Linux base install + custom window manager

References:

- Artix [Installation With Full Disk Encryption]
(https://wiki.artixlinux.org/Main/InstallationWithFullDiskEncryption)

- Arch [LUKS on a partition]
(https://wiki.archlinux.org/title/Dm-crypt/Encrypting_an_entire_system#LUKS_on_a_partition)

- The Rad Lectures [Step-by-Step Artix Linux Install]
(https://www.radleylewis.com/lab/guides/artix-openrc-install-guide/)

## Pre-installation

Boot latest ISO (OpenRC variant)

### Keyboard layout
```
loadkeys sv-latin1
```

### Identify the target disk

```
fdisk -l
```
Look for your NVMe drive, typically `/dev/nvme0n1`  
Confirm size matches expectations before proceeding

### Erase disk

```bash
 dd bs=4096 if=/dev/zero iflag=nocache of=/dev/nvme0n1 oflag=direct status=progress || true
 sync
 dd bs=4096 if=/dev/urandom iflag=nocache of=/dev/nvme0n1 oflag=direct status=progress || true
 sync
 ```

## Installation

### Partition with fdisk

Target layout (UEFI, GPT):

| Partition | Mount | Size | Type |
|-----------|-------|------|------|
| nvme0n1p1 | /boot | 1G   | EFI System |
| nvme0n1p2 | /     | rest | LUKS encrypted partition |


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
Partition number (2-128, default 3): <Enter>
First sector: <Enter>
Last sector: <Enter>    (uses all remaining space)

Command (m for help): p
(verify the table looks correct)

Command (m for help): w
```

Expected result:
```
/dev/nvme0n1p1   2048    2099199    1G      Linux filesystem
/dev/nvme0n1p2   2099200 <end>      <rest>  Linux filesystem
```

> **Note:** After writing, the kernel may not see all partitions immediately ("device busy"). Verify with `lsblk`. If pX is missing, reboot the ISO — the table is correctly on disk and all 3 will appear after reboot.

### Create the LUKS encrypted container

```bash
cryptsetup luksFormat /dev/nvme0n1p2
```

Open the container:
```bash
cryptsetup open /dev/nvme0n1p2 cryptroot
```
The decrypted container is now available at `/dev/mapper/cryptroot`

### Format

```bash
mkfs.fat -F 32 /dev/nvme0n1p1
mkfs.ext4 /dev/mapper/cryptroot
```

### Mount

```bash
mount /dev/mapper/cryptroot /mnt
mkdir -p /mnt/boot
mount /dev/nvme0n1p1 /mnt/boot
```

Verify:
```
lsblk
```

### Network

> **Use ethernet during installation.** Attempting to set up WiFi at this stage is a known failure point.

Verify connectivity:
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

For security, the standard Linux kernel and its' headers - should be replaced by a hardened version  
It is very recommended to install linux-firmware too, or some devices such as network cards may not work.  
Confirmed necessary on lenovo using i5: no wlan0 
```bash
basestrap /mnt linux-hardened linux-hardened-headers linux-firmware
#basestrap /mnt linux linux-firmware
```

and, nice to have
```bash
basestrap /mnt sudo vim
```

### Generate fstab

> **Note:** `basestrap` must have run first — it creates `/mnt/etc/`. If you get "no such file or directory", run `mkdir -p /mnt/etc` and retry.

```
fstabgen -U /mnt >> /mnt/etc/fstab
```

Inspect the result and make sure all partitions are listed (root, boot):
```
cat /mnt/etc/fstab
```

### Chroot into the new system

```bash
artix-chroot /mnt bash
```

Set up a root password
```bash
passwd
```

```bash
pacman -Sy
pacman-key --init
pacman-key --populate artix
```

### Configure the system clock
Set the time zone:
```bash
ln -sf /usr/share/zoneinfo/Europe/Stockholm /etc/localtime
```
Run hwclock to generate /etc/adjtime:
```bash
hwclock --systohc
```

### Locale

Edit `/etc/locale.gen` and uncomment `en_US.UTF-8 UTF-8`, then:
```bash
locale-gen
echo "LANG=en_US.UTF-8" > /etc/locale.conf
```

### Console keymap

```
echo 'keymap="sv-latin1"' > /etc/conf.d/keymaps
```

### mkinitcpio.conf

```bash
vim /etc/mkinitcpio.conf
```
```
HOOKS="base udev autodetect modconf block keyboard keymap consolefont encrypt filesystems fsck"
```

> **Note:** `keyboard keymap consolefont` must come *before* `encrypt` so the keyboard is active when the LUKS passphrase prompt appears at boot.

```bash
pacman -S cryptsetup cryptsetup-openrc glibc mkinitcpio
###
mkinitcpio -p linux-hardened
```

### Bootloader (GRUB)

```bash
pacman -S grub efibootmgr
````

```bash
grub-install --target=x86_64-efi --efi-directory=/boot --bootloader-id=grub
```

Append UUID to `/etc/default/grub`
```bash
blkid -s UUID -o value /dev/nvme0n1p2 >> /etc/default/grub
```
or `:r!blkid` somewhere inside `/etc/default/grub`

then replace `<uuid>` like so:
```sh
GRUB_CMDLINE_LINUX_DEFAULT="loglevel=3 quiet cryptdevice=UUID=<uuid>:cryptroot root=/dev/mapper/cryptroot"
```
```bash
grub-mkconfig -o /boot/grub/grub.cfg
```

### Add user

```
useradd -m -G wheel,video <username>
#passwd <username>
```
video needed to run `brightnessctl`

Allow wheel group to use [sudo without password](../../README#sudo_without_password)

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
pacman -S iwd iwd-openrc dbus-openrc dhcpcd dhcpcd-openrc openresolv
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

#### Resolve nameserver

If `/etc/resolv.conf` empty, add:
```
nameserver 8.8.8.8 # Primary DNS
nameserver 1.1.1.1
```
`chmod 444` so not overwritten, `chmod 644` if need edit 


#### Prevent `dhcpcd` from interfering with `wlan0`

```
echo "denyinterfaces wlan*" >> /etc/dhcpcd.conf
```
(iwd manages WiFi DHCP itself)

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

Log in as `$USER`.

### Connect to WiFi

`iwd` and `dbus` should be running (enabled in chroot)

```
iwctl device list
iwctl station wlan0 scan
iwctl station wlan0 get-networks
iwctl --password PSK station wlan0 connect YOUR_SSID
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

```
sudo pacman -S libxinerama libxss noto-fonts xorg-xinit xterm
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

##### Troubleshooting

```bash
less ~/.local/share/xorg/Xorg.0.log
```


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

## Troubleshooting

Boot from ISO, then remount and chroot:
```bash
cryptsetup luksOpen /dev/nvme0n1p2 cryptroot
mount /dev/mapper/cryptoot /mnt
```
```bash
artix-chroot /mnt bash
```

---

See [LESSONS_LEARNED.md](LESSONS_LEARNED.md) for lessons learned.
