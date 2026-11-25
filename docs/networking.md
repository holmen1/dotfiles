# Networking

## Artix / OpenRC

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

For the broader Artix OpenRC and FreeBSD rc command overview, see the
[service management guide](service-management.md).

| systemd | OpenRC |
|---|---|
| `systemctl enable <svc>` | `rc-update add <svc> default` |
| `systemctl start <svc>` | `rc-service <svc> start` |

There is no `hostnamectl` — use `hostname -s` or `/etc/hostname`. No
`systemctl --user` either; user-level daemons need another approach
(e.g. supervise-daemon, user openrc session).

## Live ISO note

The Artix live ISO uses **ConnMan** for networking, not iwd/NetworkManager.

## FreeBSD

FreeBSD configures interfaces and startup networking through `/etc/rc.conf`;
`sysrc` is the command-line tool for changing those settings. Check the
installer's existing configuration before adding entries:

```sh
ifconfig -a
cat /etc/rc.conf
netstat -rn
cat /etc/resolv.conf
```

### Wired DHCP

Replace `re0` with the actual wired interface from `ifconfig -a`. For a DHCP
network, configure the interface and restart networking:

```sh
sudo sysrc ifconfig_re0="DHCP"
sudo service netif restart
```

DHCP normally supplies the address, default route, and DNS settings.

### Wi-Fi with WPA

First identify the wireless hardware. `sysctl net.wlan.devices` lists supported
wireless parent devices; the interface name varies by driver and is not always
`wlan0`. Add a network entry to `/etc/wpa_supplicant.conf` (create the file if
needed):

```conf
network={
    ssid="your-network-name"
    psk="your-network-password"
}
```

Keep the credentials file readable only by root:

```sh
sudo chown root:wheel /etc/wpa_supplicant.conf
sudo chmod 600 /etc/wpa_supplicant.conf
```

In the following example, replace `iwm0` with the parent device reported on
your system. `wlans_iwm0` creates `wlan0` at boot; `WPA DHCP` enables WPA
association and requests network settings through DHCP.

```sh
sudo sysrc wlans_iwm0="wlan0"
sudo sysrc ifconfig_wlan0="WPA DHCP"
sudo service netif restart
```

To scan, the wireless interface must exist first:

```sh
sudo ifconfig wlan0 up list scan
```

For a hidden network, add `scan_ssid=1` inside its `network={...}` block.
Follow the FreeBSD Handbook's wireless instructions for country/regulatory
settings or other authentication methods.

### DNS and connectivity checks

With DHCP, `dhclient` normally updates `/etc/resolv.conf` using DNS information
from the network. Do not make the file immutable to keep those updates from
working. For a static network, configure the DNS servers appropriate to that
network in `/etc/resolv.conf`.

Check the interface address and default route, then test IP connectivity and
name resolution separately:

```sh
ifconfig
netstat -rn
ping -c 3 1.1.1.1
ping -c 3 freebsd.org
```

If the interface has no address, check the interface/driver and DHCP or Wi-Fi
association before changing DNS. If IP connectivity works but name resolution
does not, inspect `/etc/resolv.conf` and the DNS settings received from DHCP.

References: [FreeBSD Handbook: Network](https://docs.freebsd.org/en/books/handbook/network/),
[rc.conf(5)](https://man.freebsd.org/cgi/man.cgi?query=rc.conf&sektion=5),
[resolv.conf(5)](https://man.freebsd.org/cgi/man.cgi?query=resolv.conf&sektion=5).
