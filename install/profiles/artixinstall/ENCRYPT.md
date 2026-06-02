# encrypt

My own mix using these resources:

- Artix [Installation With Full Disk Encryption]
(https://wiki.artixlinux.org/Main/InstallationWithFullDiskEncryption)

- Arch [LUKS on a partition]
(https://wiki.archlinux.org/title/Dm-crypt/Encrypting_an_entire_system#LUKS_on_a_partition)

- The Rad Lectures [Step-by-Step Artix Linux Install]
(https://www.radleylewis.com/lab/guides/artix-openrc-install-guide/)

```
|--------------------------------------------------|
|   /boot            |  /                          |                
|                    |  /dev/mapper/cryptroot      |
|--------------------|-----------------------------|
|                    |  LUKS encrypted partition   |
|   /dev/nvme0n1p1   |  /dev/nvme0n1p2             |
|--------------------------------------------------|
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
cryptsetup open /dev/nvme0n1p2 cryptroot
```
The decrypted container is now available at /dev/mapper/cryptroot

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
basestrap /mnt linux-hardened linux-hardened-headers linux-firmware
#basestrap /mnt linux linux-firmware
```
Note: firmware resolved no wlan0 on lenovo using i5

and, nice to have
```bash
basestrap /mnt sudo
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

## Chroot into the new system

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
vi /etc/mkinitcpio.conf
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

then replace `<uuid>` like so:
```sh
GRUB_CMDLINE_LINUX_DEFAULT="loglevel=3 quiet cryptdevice=UUID=<uuid>:cryptroot root=/dev/mapper/cryptroot"
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

If `/etc/resolv.conf` empty, add:
```
nameserver 8.8.8.8 # Primary DNS
nameserver 1.1.1.1
```
`chmod 444` so not overwritten, `chmod 644` if need edit 

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


## Troubleshooting

```bash
cryptsetup luksOpen /dev/nvme0n1p2 cryptroot
mount /dev/mapper/cryptoot /mnt
```
```bash
artix-chroot /mnt bash
```

