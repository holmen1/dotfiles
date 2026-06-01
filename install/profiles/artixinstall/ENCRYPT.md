# encrypt

My own mix using these resources:

- Artix [Installation With Full Disk Encryption]
(https://wiki.artixlinux.org/Main/InstallationWithFullDiskEncryption)

- Arch [LVM on LUKS]
(https://wiki.archlinux.org/title/Dm-crypt/Encrypting_an_entire_system#LVM_on_LUKS)

- The Rad Lectures [Step-by-Step Artix Linux Install]
(https://www.radleylewis.com/lab/guides/artix-openrc-install-guide/)

```
|----------------------------------------------------------------------|
|   /boot/efi        | [SWAP]                 | /                      |                
|                    |                        |                        |
|                    | /dev/lvmSystem/volSwap | /dev/lvmSystem/volRoot |
|--------------------|------------------------|------------------------|
|                    |           LUKS encrypted partition              |
|   /dev/nvme0n1p1   |           /dev/nvme0n1p2                        |
|----------------------------------------------------------------------|
```


## Partition

### Erase disk

```bash
 dd bs=4096 if=/dev/zero iflag=nocache of=/dev/nvme0n1 oflag=direct status=progress || true
 sync
 dd bs=4096 if=/dev/urandom iflag=nocache of=/dev/nvme0n1 oflag=direct status=progress || true
 sync
 ```

### Partition with fdisk

Target layout (UEFI, GPT):

| Partition | Size |
|-----------|------|
| nvme0n1p1 | 1G   |
| nvme0n1p2 | rest |

### Create the LUKS encrypted container

```bash
cryptsetup luksFormat /dev/nvme0n1p2
```

Open the container:
```bash
cryptsetup open /dev/nvme0n1p2 cryptlvm
```
The decrypted container is now available at /dev/mapper/cryptlvm.

### Logical volumes

Create a physical volume on top of the opened LUKS container:
```bash
pvcreate /dev/mapper/cryptlvm
```

Create a volume group and add the previously created physical volume to it:
```bash
vgcreate lvmSystem /dev/mapper/cryptlvm
```

```bash
 lvcreate -L 8G -n volSwap lvmSystem
 lvcreate -l +100%FREE -n volRoot lvmSystem
 lvreduce -L -256M lvmSystem/volRoot
```

### Format

```bash
mkfs.fat -F 32 /dev/nvme0n1p1
fatlabel /dev/nvme0n1p1 ESP
mkswap -L SWAP /dev/lvmSystem/volSwap
mkfs.ext4 -L ROOT /dev/lvmSystem/volRoot
```

### Mount
```bash
swapon /dev/lvmSystem/volSwap
mount /dev/lvmSystem/volRoot /mnt
mkdir -p /mnt/boot/efi
mount /dev/disk/by-label/ESP /mnt/boot/efi
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


### Install a kernel
For security, the standard Linux kernel and its' headers - should be replaced by a hardened version:
```bash
basestrap /mnt linux-hardened linux-hardened-headers
#basestrap /mnt linux linux-firmware
```

and, nice to have
```bash
basestrap /mnt sudo
```

### Generate fstab

> **Note:** `basestrap` must have run first — it creates `/mnt/etc/`. If you get "no such file or directory", run `mkdir -p /mnt/etc` and retry.

```
fstabgen -U /mnt >> /mnt/etc/fstab
```

Inspect the result and make sure all partitions are listed (root, efi, swap):
```
cat /mnt/etc/fstab
```

## Chroot into the new system

```bash
artix-chroot /mnt /bin/bash
```

Set up a root password
```bash
passwd
```

```bash
pacman -Sy
pacman-key --init
pacman-key --populate artix
#pacman -Syu
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
vi /etc/mkinitcpio.conf
```
```
HOOKS="base udev autodetect modconf block keyboard keymap consolefont encrypt lvm2 resume filesystems fsck"
```

> **Note:** `keyboard keymap consolefont` must come *before* `encrypt` so the keyboard is active when the LUKS passphrase prompt appears at boot.

```bash
pacman -S lvm2 lvm2-openrc cryptsetup cryptsetup-openrc glibc mkinitcpio
###
mkinitcpio -p linux-hardened
```


### Bootloader (GRUB)

```bash
pacman -S grub efibootmgr
````

```bash
grub-install --target=x86_64-efi --efi-directory=/boot/efi --bootloader-id=grub --removable
```

> **Note (Lenovo X1):** Add `--removable` so GRUB is also installed to the fallback path `EFI/BOOT/BOOTX64.EFI`. Without it, some UEFI firmware (Lenovo in particular) loses the EFI boot entry on reboot and drops to a raw device menu (`NVMe0: UMIS RPET...`).

Enable GRUB cryptodisk support in `/etc/default/grub` — **must be set before `grub-install`**:
```bash
echo 'GRUB_ENABLE_CRYPTODISK=y' >> /etc/default/grub
```

Append UUID to `/etc/default/grub`
```bash
blkid -s UUID -o value /dev/nvme0n1p2 >> /etc/default/grub
blkid -s UUID -o value /dev/lvmSystem/volSwap >> /etc/default/grub
```

then replace `<uuid>` like so:
```sh
GRUB_CMDLINE_LINUX_DEFAULT="loglevel=3 quiet cryptdevice=UUID=<uuid>:cryptlvm resume=UUID=<uuid>"
#GRUB_CMDLINE_LINUX_DEFAULT="loglevel=3 quiet zswap.enabled=1 cryptdevice=UUID=<uuid>:cryptlvm root=/dev/lvmSystem/volRoot"
```
```bash
grub-mkconfig -o /boot/grub/grub.cfg
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

### Boot services
In order to decrypt and use the LUKS/LVM volumes, the following services need to be installed and activated:
```bash
rc-update add device-mapper boot
rc-update add lvm boot
rc-update add dmcrypt boot
```

### Exit chroot and reboot

```
exit
umount -R /mnt
reboot
```

Remove the ISO/USB when the machine powers off.


## Troubleshooting

```bash
cryptsetup luksOpen /dev/nvme0n1p2 cryptlvm
vgchange -ay lvmSystem  # activates all known volume groups in the system
mount /dev/lvmSystem/volRoot /mnt
```
```bash
artix-chroot /mnt /bin/bash
```

