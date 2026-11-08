# Artix install notes

Supplementary notes for the
[Artix installation guide](../install/profiles/artixinstall/README.md) —
differences from Arch, rescue path, and package strategy.

## Artix vs Arch tooling

- No guided installer (no `archinstall` equivalent) — manual `basestrap`.
- `basestrap` instead of `pacstrap`, `fstabgen` instead of `genfstab`,
  `artix-chroot` instead of `arch-chroot`.
- Set hostname in **both** `/etc/hostname` and `/etc/conf.d/hostname`
  (OpenRC needs the latter).
- Init is OpenRC — replace systemd service packages with `-openrc` variants;
  see [networking.md](networking.md#openrc-service-commands-vs-systemd) for
  the command mapping.

## ISO rescue path (if the boot path fails)

Boot from the Artix ISO, then:

```sh
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

Adjust device names to the actual layout (`cryptsetup luksOpen` on newer
versions).

## Package strategy

- Start with `packages/minimal/` — just enough to get an X server running
  (`xorg-xinit`, `xterm`, xlibre).
- Verify `startx` launches xterm before installing the full per-host
  package list (e.g. `packages/gadsden/`).
- XLibre-specific issues:
  [install/build/xlibre/README.md](../install/build/xlibre/README.md).
