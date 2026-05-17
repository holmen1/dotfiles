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

## Recovery (chroot from ISO — last resort only)
- Prefer fixing on the live system when possible (e.g. `sudo pacman -S <missing-pkg>` over WiFi)
- Use chroot only when the live system can't boot or has no network at all
- Only mount root (`/mnt`)
- Minimal chroot repair:
  ```
  mount /dev/nvme0n1p3 /mnt
  artix-chroot /mnt
  pacman -S <missing-pkg>
  exit && reboot
  ```

## Package strategy
- Start with `packages/minimal/` — just enough to get X server running (`xorg-xinit`, `xterm`, xlibre)
- Verify `startx` launches xterm before installing the full `packages/gadsden/` list
- Avoids chasing failures caused by unknown missing deps in a big install batch
- See [install/build/xlibre/LESSONS_LEARNED.md](../../build/xlibre/LESSONS_LEARNED.md) for xlibre-specific issues
