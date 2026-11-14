# Artix post-install guide

Post-install setup, Artix-specific notes, and package-management reference for
the [base installation guide](artix-install.md).

## Post-installation

> You need a working network connection before running the post-install script.
> Keep ethernet connected until WiFi is configured and verified.

Log in as `$USER`.

### Connect to WiFi

`iwd` and `dbus` should be running (enabled in chroot)

```
iwctl device list
iwctl station wlan0 scan
iwctl station wlan0 get-networks
iwctl --passphrase PSK station wlan0 connect YOUR_SSID
```

If `iwd` is not running, start it first:
```
sudo rc-service dbus start
sudo rc-service iwd start
```

Verify:
```
ip addr show wlan0        # should show inet address
ping -c 3 1.1.1.1         # tests IP routing
ping -c 3 artixlinux.org  # tests DNS
```
Recheck if `/etc/resolv.conf` empty

If DNS is not configured, temporarily add nameservers to `/etc/resolv.conf`:

```conf
nameserver 8.8.8.8
nameserver 1.1.1.1
```

The original setup used `chmod 444 /etc/resolv.conf` to prevent overwrites
(`chmod 644` to edit again). This can interfere with automatic DNS updates;
see [networking](networking.md) for the `iwd`/`openresolv` configuration.

### User and privilege setup

The base guide creates the user. To add supplementary groups later:

```sh
sudo usermod -aG input "$USER"
# Or: sudo usermod -aG audio "$USER"
```

Deleting a user's password with `passwd -d username` permits passwordless
login and is risky.

#### Autologin

For Artix, set the following in `/etc/conf.d/agetty.tty1`:

```conf
agetty_options="-a yourusername"
```

For an Arch systemd installation, the equivalent getty override is:

```conf
ExecStart=-/usr/bin/agetty --noreset --autologin yourusername --noclear - ${TERM}
```

### Sudo without password

Edit sudoers using `EDITOR=vim visudo` and enable:

```conf
%wheel ALL=(ALL:ALL) NOPASSWD: ALL
```

If `/etc/sudoers.d/00_user` contains `user ALL=(ALL) ALL`, it can override
the group policy. Remove that file if wheel-based NOPASSWD is intended.
Verify membership with `groups`.

### Clone dotfiles

#### Configure Git and SSH

```sh
git config --global user.name "$git_username"
git config --global user.email "$git_email"
ssh-keygen -t ed25519 -C "$git_email" -f ~/.ssh/id_ed25519 -N ""
eval "$(ssh-agent -s)"
ssh-add ~/.ssh/id_ed25519
chmod 600 ~/.ssh/id_ed25519
```

Add the public key to GitHub. Alternatively, reuse an existing key as below.

#### Switch to SSH (reusing existing key)

Copy your existing private key to the new machine, then:
```
chmod 700 ~/.ssh
chmod 600 ~/.ssh/id_ed25519
```

Verify the key works with GitHub:
```
ssh -T git@github.com
```
#### Clone
```
mkdir repos
cd repos
git clone git@github.com:holmen1/dotfiles.git
```
Verify:
```
git remote -v
```

For an HTTPS clone instead:

```sh
mkdir -p ~/repos && cd ~/repos
git clone https://github.com/holmen1/dotfiles.git
```

### Install

Some need to built from source

#### 0. Synchronizes the repository databases and updates the system's packages

```bash
pacman -Syu
```

#### 1. Install X server

The `besk` profile uses Xorg and the AMDGPU driver:

```
sudo pacman -S xorg-server xf86-video-amdgpu
```

Choose the appropriate video driver for the machine. The `gadsden` and `x1`
package lists still specify XLibre; consult the applicable host's package list.

#### 2. Install X stuff

```
sudo pacman -S libxinerama libxss noto-fonts xorg-xinit xterm
```

Verify by creating a minimal `.xinitrc`
```bash
echo "exec xterm" > ~/.xinitrc
```

then
```bash
startx
```
a terminal should open, may need be pointed to to activate, `pkill x` to close

##### Troubleshooting

```bash
less ~/.local/share/xorg/Xorg.0.log
```


#### 3. Build or install Haskell Compiler (GHC)

For systems requiring Haskell development or custom xmonad builds:

- [GHC build scripts](../install/build/ghc/) - Install Haskell compiler from source
- Provides `ghc`, `runhaskell`, and Cabal library for building Haskell packages

#### 4. XMonad Window Manager

Building custom xmonad without cabal-install dependency:

- [XMonad build scripts](../install/build/xmonad/) - Build xmonad and xmonad-contrib from source
- Uses `runhaskell Setup.lhs` with GHC's built-in Cabal library
- Produces standalone binaries without cabal-install dependency

#### 5. Run install script

Press enter to skip steps, `y` as shown below.
This will:
- Install pkglist
- Install `xmonad`
- Build and install `st`
- Link dotfiles
- Enable services (openrc)
- Run sanity check
```
cd ~/repos/dotfiles/install/profiles/artixinstall
./configure-build-install-link.sh
```
```bash
/home/holmen1/repos/dotfiles/install/profiles/artixinstall/packages/gadsden/pkglist.txt
Install pkglist? [y]
Install xmonad? [y]
Build st? [y]
Install st? [y]
Link dotfiles? [y]
Enable services? [y]
Run tests? [Y]
```

## Package management

Artix has its own set of official repositories which must take precedence over any other 3rd party ones

OpenRC equivalents for common packages:
- `networkmanager-openrc`
- `iwd-openrc`
- `dbus-openrc`

See the [x1 package lists](../install/profiles/artixinstall/packages/x1/) for an example.

All Arch repositories are disabled by default.
To enable them install artix-archlinux-support from the galaxy repository,
and enable repos in `/etc/pacman.conf`:
```bash
[extra]
Include = /etc/pacman.d/mirrorlist-arch
```

### Package lists

Per-host package lists are stored under
[`install/profiles/artixinstall/packages/`](../install/profiles/artixinstall/packages/).
For example, see the [x1 package lists](../install/profiles/artixinstall/packages/x1/).

### Export installed packages

```sh
pacman -Qqe > ~/repos/dotfiles/install/profiles/artixinstall/packages/<hostname>/pkglist.txt
pacman -Qqm > ~/repos/dotfiles/install/profiles/artixinstall/packages/<hostname>/foreignpkglist.txt
```

## Artix-specific notes

### Artix vs Arch tooling

- No guided installer (no `archinstall` equivalent) — manual `basestrap`.
- `basestrap` instead of `pacstrap`, `fstabgen` instead of `genfstab`,
  `artix-chroot` instead of `arch-chroot`.
- Set hostname in **both** `/etc/hostname` and `/etc/conf.d/hostname`
  (OpenRC needs the latter).
- Init is OpenRC — replace systemd service packages with `-openrc` variants;
  see [networking.md](networking.md#openrc-service-commands-vs-systemd) for
  the command mapping.

### ISO rescue path (if the boot path fails)

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

### Package strategy

- Start with just enough to get an X server running (`xorg-server`,
  `xorg-xinit`, and `xterm` for `besk`).
- Verify `startx` launches xterm before installing the full per-host
  package list (e.g. `packages/gadsden/`).
- XLibre-specific issues:
  [install/build/xlibre/README.md](../install/build/xlibre/README.md).

### Package queries

Search the synchronized package databases:

```sh
pacman -Ss string1 string2
```

List explicitly installed packages that are not dependencies:

```sh
pacman -Qet
```

View a package's dependency tree:

```sh
pactree package_name
```

List packages from a repository with `paclist`:

```sh
paclist extra
```

### Remove packages

Remove a package and dependencies not required by other installed packages:

```sh
sudo pacman -Rs package_name
```

Add `-n` (`--nosave`) to avoid saving changed configuration files as
`.pacsave` files.

### Downgrade packages

To downgrade from the local cache, find the package file and install that exact
version:

```sh
ls /var/cache/pacman/pkg | grep '^package-name-'
sudo pacman -U /var/cache/pacman/pkg/package-name-<version>-<arch>.pkg.tar.zst
```

If the version is no longer cached, download the package file and install it:

```sh
sudo pacman -U ./package-name-<version>-<arch>.pkg.tar.zst
```

Check the installed version with `pacman -Q package-name`. Keep a cached
package file if you may need it again; locally built packages can be
downgraded with `pacman -U` in the same way.

## Audio (ALSA)

Install `alsa-utils` and, if needed, `sof-firmware`:

```sh
sudo pacman -S alsa-utils
```

Diagnose with `aplay -l`, `lspci -nnk | grep -A3 -i audio`, and
`dmesg | grep -iE 'sof|snd|hda|audio|codec'`. If the analog card is not card
0, set its number in `~/.asoundrc` or `/etc/asound.conf`:

```conf
defaults.pcm.card X
defaults.ctl.card X
```

Disable Auto-Mute and unmute channels in `alsamixer -c X` if needed. For
missing Intel SOF firmware, install `sof-firmware` and `alsa-ucm-conf`.
Other useful commands:

```sh
amixer -c X
speaker-test -c 2
speaker-test -D hw:X,0 -c 2
sudo alsactl store
```

## Docker (Artix/OpenRC)

```sh
sudo pacman -S docker docker-openrc
sudo rc-update add docker default
sudo rc-service docker start
docker info
sudo usermod -aG docker "$USER"
```

After the group change takes effect, a smoke test is:

```sh
docker run -it --rm artixlinux/artixlinux:base /bin/bash -c "echo hello world"
```

## SSH server (remote login)

```sh
sudo pacman -S openssh openssh-openrc
sudo rc-update add sshd default
sudo rc-service sshd start
sudo rc-service sshd status
sudo grep sshd /var/log/auth.log
```

On Arch with systemd instead, install `openssh`, then use
`sudo systemctl enable sshd && sudo systemctl start sshd`;
check `systemctl status sshd` and `journalctl -u sshd`.

If using UFW, install it, allow SSH before enabling it, then check its status:

```sh
sudo pacman -S ufw
sudo ufw allow ssh
sudo ufw enable
sudo ufw status
```

## VPN (Mullvad)

```sh
sudo pacman -S mullvad-vpn-daemon mullvad-vpn-openrc
sudo rc-update add mullvad-daemon default
sudo rc-service mullvad-daemon start
mullvad status
mullvad account login <your_account_number>
mullvad relay list
mullvad relay set location se sto
mullvad connect
mullvad disconnect
```

## VS Code binary install

The following is the original Linux binary install example; use the archive
name and extracted directory for the version being installed:

```sh
tar -xvzf code-stable-x64-1734606550.tar.gz
sudo mv Downloads/VSCode-linux-x64/ /opt/
sudo ln -s /opt/VSCode-linux-x64/bin/code /usr/local/bin/code
```

## Further references

- Source builds: [build overview](../install/build/README.md) and [build guide](build.md).
- Shell one-liners: [shell guide](shell.md).
- X11 session startup and debugging: [X11 guide](x11.md) and [XLibre](../install/build/xlibre/README.md).

## Lessons learned

### Keep `/bin/sh` pointing to a working shell

Removing `dash` left OpenRC unable to execute its scripts because `/bin/sh`
pointed to `dash`. When changing the `/bin/sh` link, make sure it still points
to a shell that is installed and working.

To recover, boot the Artix ISO and mount the installed root at `/mnt` using
the [LUKS + LVM recovery steps](artix-install.md#troubleshooting). Confirm
that Bash is installed in the mounted system, then restore the link before
rebooting:

```sh
test -x /mnt/bin/bash || exit 1
ln -sf bash /mnt/bin/sh
```
