# install

Guides for setting up environments quickly and consistently — Linux, BSD, macOS.

## Documentation map

- Full docs index: [docs/README.md](../docs/README.md)
- Detailed post-install operations: [docs/postinstall.md](../docs/postinstall.md)
- Reusable shell one-liners: [docs/shell.md](../docs/shell.md)
- X11 startup/debugging/troubleshooting entry point: [docs/x11.md](../docs/x11.md)
- Build conventions and component map: [docs/build.md](../docs/build.md)

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

- **Artix (full install):** [docs/artix-install-guide.md](../docs/artix-install-guide.md)
- **FreeBSD:** [docs/bsd-install-guide.md](../docs/bsd-install-guide.md)
- **macOS config:** [install/profiles/macinstall/README.md](./profiles/macinstall/README.md)

## Post-install (quick links)

- Connectivity, users/sudo, git/ssh, docker, VPN, remote SSH server, and admin tips:
  [docs/postinstall.md](../docs/postinstall.md)
- X11 session startup/display manager/debugging:
  [docs/x11.md](../docs/x11.md)
- Component builds:
  [install/build/README.md](./build/README.md) and [docs/build.md](../docs/build.md)
