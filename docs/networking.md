# Networking (Artix / OpenRC)

Canonical networking rules for Artix with OpenRC. Distilled from install
incidents — this is the working setup, not history.

## iwd (WiFi)

- `iwd` associates to WiFi but **does not assign an IP by default**.
- Its default DNS target is `systemd-resolved` — wrong on OpenRC.
- It only manages WiFi interfaces; wired ethernet needs `dhcpcd`.

Working `/etc/iwd/main.conf`:

```ini
[General]
EnableNetworkConfiguration=true

[Network]
NameResolvingService=resolvconf
```

Requires the `openresolv` package for `NameResolvingService=resolvconf`.

## dhcpcd (wired + DHCP)

- `dhcpcd` manages **all** interfaces by default.
- If WiFi drops after ethernet disconnects, `dhcpcd` is fighting `iwd` on
  `wlan0`. Constrain it in `/etc/dhcpcd.conf`:

```conf
denyinterfaces wlan*
```

## Get ethernet working post-install (no chroot needed)

If already booted with WiFi working (`iwd` running with
`EnableNetworkConfiguration=true`):

```sh
sudo pacman -S dhcpcd dhcpcd-openrc openresolv
sudo rc-update add dhcpcd default
sudo rc-service dhcpcd start
# eth0 should get an IP immediately
```

Then set `NameResolvingService=resolvconf` in `/etc/iwd/main.conf` and restart
`iwd` for permanent DNS.

## OpenRC service commands (vs systemd)

| systemd | OpenRC |
|---|---|
| `systemctl enable <svc>` | `rc-update add <svc> default` |
| `systemctl start <svc>` | `rc-service <svc> start` |

There is no `hostnamectl` — use `hostname -s` or `/etc/hostname`. No
`systemctl --user` either; user-level daemons need another approach
(e.g. supervise-daemon, user openrc session).

## Live ISO note

The Artix live ISO uses **ConnMan** for networking, not iwd/NetworkManager.
