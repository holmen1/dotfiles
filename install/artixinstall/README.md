# artixinstall

Port of archinstall to Artix Linux (OpenRC). Artix does not have a guided installer — every step is manual.

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

Inspect the result and make sure all four partitions are listed (EFI, swap, root, home):
```
cat /mnt/etc/fstab
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

```
passwd
```

```
useradd -m -G wheel holmen1
passwd holmen1
```

Allow wheel group to use sudo — run `visudo` and uncomment:
```
%wheel ALL=(ALL:ALL) ALL
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

### Install xlibre (before running install script)

`xlibre-xserver` is in AUR and requires a bootstrap step to avoid conflicts with `xorg-server`. Do this manually before the install script, which cannot handle the required sequencing:

```
yay -S xlibre-xserver-bootstrap    # provides xorg-server, satisfies deps
yay -S xlibre-input-libinput
yay -S xlibre-xserver              # replaces bootstrap; confirm removal when prompted
```

Verify no stale xorg packages remain:
```
pacman -Q | grep 'xorg-server\|xf86-'
pacman -Q | grep 'xlibre-'
```

The install script's `foreignpkglist.txt` lists `xlibre-xserver` and `xlibre-input-libinput` — yay will skip them as already installed.

### Run install script

```
cd ~/repos/dotfiles/install/artixinstall
sh configure_build_install_link.sh
```

This will prompt to:
1. Configure git
2. Generate SSH key (add public key to GitHub before cloning via SSH)
3. Install yay (AUR helper)
4. Install pkglist / foreignpkglist
5. Link dotfiles
6. Enable services (openrc)
7. Run sanity check

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
  ~/repos/dotfiles/install/artixinstall/packages/gadsden/
```

### Sanity check

```
~/repos/dotfiles/install/artixinstall/tests/gadsden/sanity_check.sh
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
