# Simple Artix installation

This guide installs Artix with OpenRC on an **x86-64 UEFI** machine using GPT,
one EFI partition, and one ext4 root partition. `/home` is part of root.

> Back up anything you need first. The examples below use `/dev/nvme0n1`,
> `/dev/nvme0n1p1`, and `/dev/nvme0n1p2`; replace **every occurrence** with
> the correct disk and partition names for your machine. A SATA disk usually
> uses names such as `/dev/sda`, `/dev/sda1`, and `/dev/sda2`.

## 1. Boot and prepare

Boot the Artix OpenRC live ISO in UEFI mode. Confirm that the following
directory exists; if it does not, reboot the ISO in UEFI mode before
continuing:

```sh
ls /sys/firmware/efi/efivars
```

If needed, select a console keyboard layout, for example Swedish:

```sh
loadkeys sv-latin1
```

Connect to the internet; wired Ethernet is simplest. Verify both connectivity and DNS:

```sh
ping -c 3 artixlinux.org
```

If that fails, configure the live connection before proceeding. See the
[networking guide](networking.md#live-iso-note).

## 2. Identify and partition the disk

Inspect disk names and sizes:

```sh
lsblk -o NAME,SIZE,TYPE,FSTYPE,MOUNTPOINTS,MODEL
```

Identify the target disk by its size and model.

```sh
fdisk /dev/nvme0n1
```

In the interactive prompt:

1. Enter `g` to create a new GPT partition table.
2. Enter `n` to create partition 1. Accept the default first sector and enter
   `+1G` for the last sector.
3. Enter `t`, select partition 1 if prompted, and choose **EFI System** from
   the type list (`L` displays available types; use its listed number).
4. Enter `n` to create partition 2, accepting the defaults to use the
   remaining disk space.
5. Enter `p` and verify the target disk, partition sizes, and EFI System type.
   Enter `w` only when correct. Enter `q` instead to quit without writing.

Check that both partitions appear with `lsblk -f`. If they do not, resolve
that before formatting; a reboot of the live ISO may be needed.

## 3. Format and mount

Double-check the names: **these commands erase the selected partitions**.

```sh
mkfs.fat -F 32 /dev/nvme0n1p1
mkfs.ext4 /dev/nvme0n1p2
mount /dev/nvme0n1p2 /mnt
mkdir -p /mnt/boot
mount /dev/nvme0n1p1 /mnt/boot
```

Confirm that the root partition is mounted at `/mnt` and the EFI partition at
`/mnt/boot` with `lsblk -f`.

## 4. Install the base system

Synchronize the live system clock before installing:

```sh
rc-service chrony start
```

Install the base system, kernel, firmware, bootloader tools, and networking
packages. This uses the hardened kernel, as in the main Artix guide:

```sh
basestrap /mnt base base-devel openrc elogind-openrc linux \
  linux-headers linux-firmware grub efibootmgr \
  iwd iwd-openrc dbus-openrc dhcpcd dhcpcd-openrc openresolv \
  sudo vi git openssh
```

Generate and inspect the filesystem table:

```sh
fstabgen -U /mnt >> /mnt/etc/fstab
cat /mnt/etc/fstab
```

Check that it lists the root filesystem and `/boot`; this layout has no
separate `/home` or swap entry. Then enter the installed system:

```sh
artix-chroot /mnt bash
```

## 5. Configure the installed system

The following commands run **inside the chroot**, as root.

Set the root password and initialize the package keyring:

```sh
passwd
pacman-key --init
pacman-key --populate artix
```

Choose your timezone; this example uses Stockholm:

```sh
ln -sf /usr/share/zoneinfo/Europe/Stockholm /etc/localtime
hwclock --systohc
```

Edit `/etc/locale.gen` with `vi`, uncomment `en_US.UTF-8 UTF-8`, then run:

```sh
locale-gen
echo 'LANG=en_US.UTF-8' > /etc/locale.conf
```

For a Swedish console keyboard, also set:

```sh
echo 'keymap="sv-latin1"' > /etc/conf.d/keymaps
```

Choose a hostname. Replace `artix` consistently in the following examples:

```sh
echo artix > /etc/hostname
echo 'hostname="artix"' > /etc/conf.d/hostname
```

Edit `/etc/hosts` to include:

```text
127.0.0.1 localhost
::1       localhost
127.0.1.1 artix.localdomain artix
```

### Networking

Enable the services installed above. `iwd` handles WiFi; `dhcpcd` handles
wired Ethernet.

```sh
rc-update add dbus default
rc-update add iwd default
rc-update add dhcpcd default
mkdir -p /etc/iwd
```

Create `/etc/iwd/main.conf` with:

```ini
[General]
EnableNetworkConfiguration=true

[Network]
NameResolvingService=resolvconf
```

Add `denyinterfaces wlan*` to `/etc/dhcpcd.conf` so wired DHCP does not
interfere with iwd's WiFi configuration:

```sh
echo 'denyinterfaces wlan*' >> /etc/dhcpcd.conf
```

See [Artix networking](networking.md) for details and troubleshooting.

### Create a user

Replace `youruser` in both commands with the username you want:

```sh
useradd -m -G wheel,video youruser
passwd youruser
```

Use `visudo` to enable sudo for the `wheel` group. Uncomment a suitable
`%wheel` rule; use the password-required rule unless you intentionally want
passwordless sudo.

### Install the bootloader

Install GRUB to the mounted EFI
partition and generate its configuration:

```sh
grub-install --target=x86_64-efi --efi-directory=/boot --bootloader-id=grub
grub-mkconfig -o /boot/grub/grub.cfg
```

If `grub-install` reports that EFI variables are unavailable, check that the
ISO was booted in UEFI mode. Do not reboot until the bootloader command
succeeds.

## 6. Reboot and continue

Exit the chroot, unmount the target filesystems, and reboot:

```sh
exit
umount -R /mnt
reboot
```
