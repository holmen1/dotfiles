# Service management

Artix and FreeBSD both use rc-style service management, but FreeBSD does not
use OpenRC. The commands, service names, and configuration differ; do not use
OpenRC commands on FreeBSD.

## Artix: OpenRC

OpenRC starts services and manages which runlevels start them. Installing a
service does not necessarily start it or enable it at boot. Artix packages
often provide an `-openrc` variant with the OpenRC service scripts.

```sh
sudo rc-service <service> status
sudo rc-service <service> start
sudo rc-service <service> stop
sudo rc-service <service> restart
```

The `start`, `stop`, and `restart` actions change the running state; they do
not enable startup at boot. To enable or disable a service in the normal
`default` runlevel, or inspect runlevel assignments:

```sh
sudo rc-update add <service> default
sudo rc-update del <service> default
rc-update show
```

Use the service's name, which may differ from its package name. Service
scripts are in `/etc/init.d/`; service-specific settings commonly live in
`/etc/conf.d/<service>`.

For example, start D-Bus now and enable it for future boots:

```sh
sudo rc-service dbus start
sudo rc-update add dbus default
```

## FreeBSD: rc

FreeBSD uses its own rc system, not OpenRC. Base service scripts are in
`/etc/rc.d/`, and installed packages commonly put theirs in
`/usr/local/etc/rc.d/`. Persistent service settings are in `/etc/rc.conf`;
use `sysrc` to change them.

```sh
sudo service <service> status
sudo service <service> start
sudo service <service> stop
sudo service <service> restart
sudo sysrc <service>_enable=YES
sudo sysrc <service>_enable=NO
```

`service` operates on the running system; `sysrc` changes whether a service
starts at boot. For example:

```sh
sudo sysrc sshd_enable=YES
sudo service sshd start
```

Some tasks use a different service and configuration on each OS. For wired
DHCP, Artix commonly runs `dhcpcd`; FreeBSD configures the interface in
`/etc/rc.conf` (for example with `sysrc ifconfig_re0="DHCP"`) and applies it
with `service netif restart`. See the [networking guide](networking.md) for
network-specific setup.
