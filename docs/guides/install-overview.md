# install

Guides for setting up environments quickly and consistently — Linux, BSD, macOS.

## Documentation map

- Full docs index: [docs/README.md](../README.md)
- Detailed post-install operations: [Artix](../artix-post-install.md) and [FreeBSD](../bsd-install.md#post-install)
- Reusable shell one-liners: [docs/shell.md](../shell.md)
- X11 startup/debugging/troubleshooting entry point: [docs/x11.md](../x11.md)
- Build conventions and component map: [docs/build.md](../build.md)

## Pre-install

```bash
# Download ISO and signature
wget image.iso
wget image.iso.sig

# Import distributor key (one-time)
gpg --recv-keys DISTRIBUTOR_KEYID

# Verify signature
gpg --verify image.iso.sig image.iso
gpg --auto-key-retrieve --verify artix-base-openrc-20260402-x86_64.iso.sig artix-base-openrc-20260402-x86_64.iso
```

macOS USB prep example:

```bash
diskutil list
diskutil unmountDisk /dev/disk4
sudo dd bs=4M if=artix-base-openrc-20260402-x86_64.iso of=/dev/disk4 status=progress oflag=sync
```

### TTY

```bash
loadkeys sv-latin1  # sv + -> us -
TERM=linux-m        # monochrome
setfont /usr/share/kbd/consolefonts/solar24x32.psfu.gz # larger font
setfont -R          # reset
```

List installed terminfo: `toe [-a]`

## Base install

- **Artix (base install):** [docs/artix-install.md](../artix-install.md)
- **FreeBSD:** [docs/bsd-install.md](../bsd-install.md)
- **macOS config:** [install/hosts/macOS/README.md](macos.md)

## Post-install (quick links)

- Connectivity, users/sudo, git/ssh, docker, VPN, remote SSH server, and admin tips:
  [Artix post-install](../artix-post-install.md) and [FreeBSD post-install](../bsd-install.md#post-install)
- X11 session startup/display manager/debugging:
  [docs/x11.md](../x11.md)
- Component builds:
  [install/build/README.md](../../install/build/README.md) and [docs/build.md](../build.md)
