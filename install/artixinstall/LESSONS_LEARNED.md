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
