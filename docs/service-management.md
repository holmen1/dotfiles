# Service management

Artix and FreeBSD both use rc-style service management, but FreeBSD does not
use OpenRC. The commands, service names, and configuration differ; do not use
OpenRC commands on FreeBSD.

## Commands

| Action | Artix (OpenRC) | FreeBSD (rc) |
| --- | --- | --- |
| Status | `sudo rc-service <service> status` | `sudo service <service> status` |
| Start now | `sudo rc-service <service> start` | `sudo service <service> start` |
| Stop now | `sudo rc-service <service> stop` | `sudo service <service> stop` |
| Restart now | `sudo rc-service <service> restart` | `sudo service <service> restart` |
| Enable at boot | `sudo rc-update add <service> default` | `sudo sysrc <service>_enable=YES` |
| Disable at boot | `sudo rc-update del <service> default` | `sudo sysrc <service>_enable=NO` |
| Inspect | `rc-update show` | `service -e` |

Starting, stopping, and restarting change the running state, not whether a
service starts at boot. `rc-update show` displays runlevel assignments;
`service -e` lists enabled FreeBSD service scripts.

## Artix: OpenRC

OpenRC manages which runlevels start services. Installing a service does not
necessarily start it or enable it at boot. Artix packages often provide an
`-openrc` variant with the OpenRC service scripts.

Use the service's name, which may differ from its package name. Service
scripts are in `/etc/init.d/`; service-specific settings commonly live in
`/etc/conf.d/<service>`.

For example, start D-Bus now with `sudo rc-service dbus start`, and enable it
for future boots with `sudo rc-update add dbus default`.

## FreeBSD: rc

FreeBSD uses its own rc system, not OpenRC. Base service scripts are in
`/etc/rc.d/`, and installed packages commonly put theirs in
`/usr/local/etc/rc.d/`. Persistent service settings are in `/etc/rc.conf`;
use `sysrc` to change them.

`service` operates on the running system; `sysrc` changes whether a service
starts at boot. For example, `sudo sysrc sshd_enable=YES` enables SSH at boot,
and `sudo service sshd start` starts it now.

Some tasks use a different service and configuration on each OS. For wired
DHCP, Artix commonly runs `dhcpcd`; FreeBSD configures the interface in
`/etc/rc.conf` (for example with `sysrc ifconfig_re0="DHCP"`) and applies it
with `service netif restart`. See the [networking guide](networking.md) for
network-specific setup.
