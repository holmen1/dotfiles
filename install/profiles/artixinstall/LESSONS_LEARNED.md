# LESSONS_LEARNED

## Installation
- No guided installer (no `artixinstall` command) — manual process using `basestrap`
- Live ISO uses ConnMan for networking, not iwd/NetworkManager
- `basestrap` instead of `pacstrap`, `fstabgen` instead of `genfstab`, `artix-chroot` instead of `arch-chroot`
- Set hostname in both `/etc/hostname` AND `/etc/conf.d/hostname` (openrc needs the latter)

## artix vs arch
- artix uses openrc; replace systemd service packages with openrc variants
- `rc-update add <service> default` instead of `systemctl enable`
- `rc-service <service> start` instead of `systemctl start`
- No `hostnamectl` — use `hostname -s` or `/etc/hostname`
- No `systemctl --user` — user-level daemons need another approach (e.g. supervise-daemon, user openrc session)

## Networking
- `iwd` associates to WiFi but **does not assign an IP by default** — requires `EnableNetworkConfiguration=true` in `/etc/iwd/main.conf`
- `iwd`'s default DNS target is `systemd-resolved` (wrong for OpenRC) — set `NameResolvingService=resolvconf` and install `openresolv`
- `iwd` only manages WiFi interfaces; wired ethernet needs `dhcpcd`
- `dhcpcd` manages **all** interfaces by default — add `denyinterfaces wlan*` to `/etc/dhcpcd.conf` or it will conflict with iwd's DHCP on `wlan0` (WiFi drops after ethernet disconnect)
- Full working `/etc/iwd/main.conf`:
  ```ini
  [General]
  EnableNetworkConfiguration=true

  [Network]
  NameResolvingService=resolvconf
  ```

### Get ethernet working post-install (no chroot needed)
If already booted with WiFi working (iwd running with `EnableNetworkConfiguration=true`):
```
sudo pacman -S dhcpcd dhcpcd-openrc openresolv
sudo rc-update add dhcpcd default
sudo rc-service dhcpcd start
# eth0 should get an IP immediately
```
Then update `/etc/iwd/main.conf` to add `NameResolvingService=resolvconf` and restart iwd for permanent DNS.

### ISO rescue path (if current boot path fails)
```
cryptsetup open /dev/nvme0n1p2 cryptlvm
vgchange -ay lvmSystem
mount /dev/lvmSystem/volRoot /mnt
mount /dev/nvme0n1p1 /mnt/boot
fstabgen -U /mnt > /mnt/etc/fstab
artix-chroot /mnt /bin/bash
mkinitcpio -p linux-hardened
grub-install --target=x86_64-efi --efi-directory=/boot --bootloader-id=grub
grub-mkconfig -o /boot/grub/grub.cfg
exit
umount -R /mnt
reboot
```

## Boot (GRUB / UEFI)

- Simplest setup for this guide: mount the ESP on `p1` at `/boot` and keep `/` on encrypted LUKS/LVM
- That gives a single passphrase prompt at boot because GRUB and the kernel live on the unencrypted ESP
- Avoid the encrypted-`/boot` + cryptodisk + keyfile path unless you explicitly want extra boot complexity

## Package strategy
- Start with `packages/minimal/` — just enough to get X server running (`xorg-xinit`, `xterm`, xlibre)
- Verify `startx` launches xterm before installing the full `packages/gadsden/` list
- Avoids chasing failures caused by unknown missing deps in a big install batch
- See [install/build/xlibre/LESSONS_LEARNED.md](../../build/xlibre/LESSONS_LEARNED.md) for xlibre-specific issues
