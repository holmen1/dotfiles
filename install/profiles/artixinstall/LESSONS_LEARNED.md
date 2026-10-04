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

## Framework AMD (`besk`) display

If X applications draw only after touchpad movement, the shell and keyboard
are not blocked. Do not change input groups, input drivers, the kernel, or
session daemons to address this symptom.

The confirmed Besk workaround is `amdgpu.dcdebugmask=0x610`, which disables
PSR, PSR selective update, and Panel Replay. Add it to the existing
`GRUB_CMDLINE_LINUX_DEFAULT`, regenerate the GRUB configuration, and reboot.
Do not keep `0x10`: it disables only PSR and did not fix the issue. The full
mask has a small idle-power cost; do not narrow it without testing.

Confirm the new boot with `grep amdgpu /proc/cmdline`; the corresponding
`/sys/module/amdgpu/parameters/dcdebugmask` value is `1552`.

## Framework AMD (`besk`) RDSEED32

`RDSEED32 is broken. Disabling the corresponding CPUID bit.` is a mitigation,
not a failed boot. Some Zen 5 CPUs can return invalid 16-bit or 32-bit
`RDSEED` values; Linux clears the affected capability until the BIOS supplies
the required microcode.

Update the Framework BIOS, reboot, and check the running revision with
`grep microcode /proc/cpuinfo`. Do not hide the message with `loglevel` or add
`clearcpuid=rdseed`; the kernel has already applied the safe workaround.

## Framework AMD (`besk`) touchpad tap-to-click (X11)

To enable tap-to-click persistently with X11/libinput, create
`/etc/X11/xorg.conf.d/90-touchpad.conf` with:

```conf
Section "InputClass"
  Identifier "touchpad tap-to-click"
  MatchIsTouchpad "on"
  Driver "libinput"
  Option "Tapping" "on"
EndSection
```

Restart the X session for the setting to take effect. For a temporary test,
`xinput` is provided by the `xorg-xinput` package; tap support can be toggled
with the device's `libinput Tapping Enabled` property.

## Package strategy
- Start with `packages/minimal/` — just enough to get X server running (`xorg-xinit`, `xterm`, xlibre)
- Verify `startx` launches xterm before installing the full `packages/gadsden/` list
- See [install/build/xlibre/LESSONS_LEARNED.md](../../build/xlibre/LESSONS_LEARNED.md) for xlibre-specific issues
