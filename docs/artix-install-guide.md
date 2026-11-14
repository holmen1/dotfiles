# Artix installation guide

Artix Linux base installation with LVM on LUKS.

Printable step-by-step checklist: [CHECKLIST.md](../install/profiles/artixinstall/CHECKLIST.md)

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
| nvme0n1p2 | LUKS container | rest | Linux filesystem |


```
fdisk /dev/nvme0n1
```
If warning about active swap partitions, do `swapoff -a`

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

### Create LVM volumes inside LUKS (root + swap + home)

Install LVM tooling (`lvm2`) in the live ISO if needed

Create PV/VG/LVs:
```bash
pvcreate /dev/mapper/cryptroot
vgcreate vg0 /dev/mapper/cryptroot

# Adjust sizes as needed:
lvcreate -L 80G -n root vg0
lvcreate -L 16G -n swap vg0
lvcreate -l 100%FREE -n home vg0
```

Your logical volumes should now be located in `/dev/vg0/`. If you cannot find them,
use the next commands to bring up the module for creating device nodes and to make volume groups available:
```
# modprobe dm_mod
# vgscan
# vgchange -ay
```

### Format

```bash
mkfs.fat -F 32 /dev/nvme0n1p1
mkfs.ext4 /dev/vg0/root
mkfs.ext4 /dev/vg0/home
mkswap /dev/vg0/swap
```

### Mount

```bash
mount /dev/vg0/root /mnt
mkdir -p /mnt/home
mount /dev/vg0/home /mnt/home
mkdir -p /mnt/boot
mount /dev/nvme0n1p1 /mnt/boot
swapon /dev/vg0/swap
```

Verify:
```
$ lsblk
NAME           MAJ:MIN RM   SIZE RO TYPE  MOUNTPOINTS
nvme0n1        259:0    0 476.9G  0 disk
├─nvme0n1p1    259:1    0     1G  0 part  /boot
└─nvme0n1p2    259:2    0 475.9G  0 part
  └─cryptroot  254:0    0 475.9G  0 crypt
    ├─vg0-root 254:1    0    80G  0 lvm   /
    ├─vg0-swap 254:2    0    16G  0 lvm   [SWAP]
    └─vg0-home 254:3    0 379.9G  0 lvm   /home
```

### Network

Verify connectivity:
```
ping -c 3 artixlinux.org
```

### Update the system clock
Activate the NTP daemon to synchronize the computer's real-time clock:

```bash
rc-service chrony start

# Legacy pre spring 2026
# rc-service ntpd start
```

### Install base system

```
basestrap /mnt base base-devel openrc elogind-openrc lvm2
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

and, nice to have during install
```bash
basestrap /mnt vi git openssh
```

### Generate fstab

> **Note:** `basestrap` must have run first — it creates `/mnt/etc/`. If you get "no such file or directory", run `mkdir -p /mnt/etc` and retry.

```
fstabgen -U /mnt >> /mnt/etc/fstab
```
Inspect the result and make sure all are listed (`/`, `/home`, `/boot`, and swap):

---
### Chroot into the new system

```bash
artix-chroot /mnt bash
```

Set up a root password
```bash
passwd
```
Initializing the keyring:
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
Add `encrypt` and `lvm2` to HOOKS:
```bash
HOOKS=(base udev autodetect microcode modconf kms keyboard keymap consolefont encrypt lvm2 block filesystems fsck)
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
GRUB_CMDLINE_LINUX_DEFAULT="loglevel=3 quiet cryptdevice=UUID=<uuid>:cryptroot root=/dev/vg0/root [amdgpu.dcdebugmask=0x610]"
```
amdgpu quirk, see: [docs/hardware.md](hardware.md)
```bash
grub-mkconfig -o /boot/grub/grub.cfg
```

### Add user

```
useradd -m -G wheel,video <username>
#passwd <username>
```
video [maybe] needed to run `xbacklight` [TODO]

Allow wheel group to use [sudo without password](postinstall.md#sudo-without-password)

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

## Troubleshooting

### Recover the LUKS + LVM root

Boot from the Artix ISO and identify the encrypted partition with `lsblk -f`.
For the layout in this guide, unlock it and activate the LVM volume group:

```bash
cryptsetup open /dev/nvme0n1p2 cryptroot
vgscan
vgchange -ay
```

The message `unknown filesystem type 'LVM2_member'` means you tried to mount
the unlocked LUKS mapping itself. That mapping is an LVM physical volume, not a
mountable filesystem; after activating the volume group, mount its root logical
volume instead:

```sh
mount /dev/vg0/root /mnt
mkdir -p /mnt/home /mnt/boot
mount /dev/vg0/home /mnt/home
mount /dev/nvme0n1p1 /mnt/boot
artix-chroot /mnt /bin/bash
```

If your setup uses different device, volume-group, or logical-volume names,
substitute those shown by `lsblk -f`, `pvs`, `vgs`, and `lvs`.

## References

- Artix [Installation With Full Disk Encryption]
(https://wiki.artixlinux.org/Main/InstallationWithFullDiskEncryption)

- Arch [LVM on LUKS]
(https://wiki.archlinux.org/title/Dm-crypt/Encrypting_an_entire_system#LVM_on_LUKS)

- Arch [Install Arch Linux on LVM]
(https://wiki.archlinux.org/title/Install_Arch_Linux_on_LVM)

- The Rad Lectures [Step-by-Step Artix Linux Install]
(https://www.radleylewis.com/lab/guides/artix-openrc-install-guide/)

## Further notes

- Post-installation and Artix-specific notes:
  [docs/artix-post-install.md](artix-post-install.md)
- Networking (iwd/dhcpcd/OpenRC): [docs/networking.md](networking.md)
- Machine-specific quirks: [docs/hardware.md](hardware.md)
- Archived incident history:
  [docs/archive/artixinstall-LESSONS_LEARNED.md](archive/artixinstall-LESSONS_LEARNED.md)
