# LESSONS_LEARNED

## artix vs arch
- artix uses openrc; replace systemd service packages with openrc variants
- `rc-update add <service> default` instead of `systemctl enable`
- `rc-service <service> start` instead of `systemctl start`
- No `systemctl --user` — user-level daemons need another approach (e.g. supervise-daemon, user openrc session)
- `artix-chroot` instead of `arch-chroot` in repair/recovery
