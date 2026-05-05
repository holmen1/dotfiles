# artixinstall

Port of archinstall to Artix Linux (OpenRC). Artix does not have a guided installer — every step is manual.

## Pre-installation

Boot the latest Artix Linux ISO (OpenRC variant).

### Keyboard layout
```
loadkeys sv-latin1
```

### Connect to WiFi

The live ISO uses ConnMan:
```
connmanctl
connmanctl> enable wifi
connmanctl> scan wifi
connmanctl> services
```
Copy the service ID from the output (looks like `wifi_<mac>_<ssid>_managed_psk`), then:
```
connmanctl> connect wifi_<id>
connmanctl> quit
```
Verify connectivity:
```
ping -c 3 artixlinux.org
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
| nvme0n1p3 | / | 50G | Linux filesystem |
| nvme0n1p4 | /home | rest | Linux filesystem |

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
Last sector: +50G

Command (m for help): n
Partition number (4-128, default 4): <Enter>
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
/dev/nvme0n1p3  18874368 123731967  50G  Linux filesystem
/dev/nvme0n1p4 123731968 <end>      <rest>  Linux filesystem
```

> **Note:** After writing, the kernel may not see all partitions immediately ("device busy"). Verify with `lsblk`. If p4 is missing, reboot the ISO — the table is correctly on disk and all 4 will appear after reboot.

### Format partitions

Labels (`-L` / `-n`) make the partitions identifiable by name instead of device path:

```
mkfs.fat -F 32 -n EFI /dev/nvme0n1p1    # EFI system partition
mkswap -L swap /dev/nvme0n1p2            # swap
mkfs.ext4 -L root /dev/nvme0n1p3         # root
mkfs.ext4 -L home /dev/nvme0n1p4         # home
```

### Mount

```
mount /dev/nvme0n1p3 /mnt
mkdir -p /mnt/boot/efi
mount /dev/nvme0n1p1 /mnt/boot/efi
mkdir -p /mnt/home
mount /dev/nvme0n1p4 /mnt/home
swapon /dev/nvme0n1p2
```

Verify:
```
lsblk
free -h    # swap should appear
```

### Install base system

```
basestrap /mnt base base-devel sudo openrc elogind-openrc dhcpcd linux linux-firmware git vi openssh
```

- `openrc` — init system
- `elogind-openrc` — session/seat management (replaces systemd-logind)

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

### Timezone

```
ln -sf /usr/share/zoneinfo/Europe/Stockholm /etc/localtime
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

### Hostname

```
echo gadsden > /etc/hostname
echo 'hostname="gadsden"' > /etc/conf.d/hostname
```

Configure `/etc/hosts`:
```
printf '127.0.0.1\tlocalhost\n::1\t\tlocalhost\n127.0.1.1\tgadsden.localdomain\tgadsden\n' >> /etc/hosts
```

### Root password

```
passwd
```

### Create user

```
useradd -m -G wheel holmen1
passwd holmen1
```

Allow wheel group to use sudo — run `visudo` and uncomment:
```
%wheel ALL=(ALL:ALL) ALL
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

### Exit chroot and reboot

```
exit
umount -R /mnt
reboot
```

Remove the ISO/USB when the machine powers off.

---

## Post-installation

> **Connect ethernet** before logging in — wifi is not configured yet and the script needs internet to install packages.

Log in as `holmen1`.

### Clone dotfiles

```
mkdir repos
cd repos
git clone https://github.com/holmen1/dotfiles.git
```
(Switch remote to SSH after keys are set up.)

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

### Enable services

```
sudo rc-update add iwd default
sudo rc-update add dbus default
sudo rc-service iwd start
```

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
