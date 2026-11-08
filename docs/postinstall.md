# Post-install guide

Common post-install tasks that are not distro-installer specific. Keep
`install/README.md` concise and use this page for details.

## Connectivity

### Confirm WiFi

```sh
ping -c 3 artixlinux.org
```

Troubleshooting:

```sh
ip addr show wlan0        # should show inet address
ping -c 3 1.1.1.1         # tests IP routing
ping -c 3 artixlinux.org  # tests DNS
```

If `/etc/resolv.conf` is empty, add:

```conf
nameserver 8.8.8.8
nameserver 1.1.1.1
```

Use `chmod 444 /etc/resolv.conf` to prevent overwrites (`644` to edit again).

### Artix + OpenRC networking

Full canonical networking rules (`iwd`, `dhcpcd`, `openresolv`, service
commands) live in [docs/networking.md](networking.md).

## User + privilege setup

### Add user

```sh
useradd -m -G wheel[,video] username
# passwd username
```

Add group membership later if needed:

```sh
sudo usermod -aG input "$USER"
# or audio
```

### Passwordless login (optional, risky)

```sh
passwd -d username
```

### Autologin

Artix (`/etc/conf.d/agetty.tty1`):

```conf
agetty_options="-a yourusername"
```

Arch systemd (if relevant):

```conf
ExecStart=-/usr/bin/agetty --noreset --autologin yourusername --noclear - ${TERM}
```

FreeBSD variant: see the [FreeBSD install guide](../install/profiles/bsdinstall/README.md#autologin).

### Sudo without password

Edit sudoers via `EDITOR=vim visudo` and enable:

```conf
%wheel ALL=(ALL:ALL) NOPASSWD: ALL
```

If `/etc/sudoers.d/00_user` contains `user ALL=(ALL) ALL`, it can override the
group policy. Remove that file if wheel-based NOPASSWD is intended.

Verify wheel membership:

```sh
groups
```

## Git + SSH bootstrap

### Configure Git

```sh
git config --global user.name "$git_username"
git config --global user.email "$git_email"
```

### SSH key setup

```sh
ssh-keygen -t ed25519 -C "$git_email" -f ~/.ssh/id_ed25519 -N ""
eval "$(ssh-agent -s)"
ssh-add ~/.ssh/id_ed25519
chmod 600 ~/.ssh/id_ed25519
```

Then add the public key to GitHub.

### Clone this repo

```sh
mkdir -p ~/repos && cd ~/repos
git clone https://github.com/holmen1/dotfiles.git
```

## Build from source

For software not available from package managers, see:
[install/build/README.md](../install/build/README.md) and [docs/build.md](build.md).

## Audio (ALSA)

Install:

```sh
pacman -S alsa-utils [sof-firmware]
```

Quick diagnostics:

1. `aplay -l`
2. `lspci -nnk | grep -A3 -i audio`
3. `dmesg | grep -iE 'sof|snd|hda|audio|codec'`

If analog card is not card 0:

```conf
# ~/.asoundrc (or /etc/asound.conf)
defaults.pcm.card X
defaults.ctl.card X
```

Common fixes:

| Problem | Fix |
|---|---|
| Auto-Mute enabled | Disable in `alsamixer -c X` and unmute channels |
| Wrong default card | Set `defaults.pcm.card` and `defaults.ctl.card` |
| Missing SOF firmware (Intel) | `sudo pacman -S sof-firmware alsa-ucm-conf` |

Useful commands:

```sh
alsamixer -c X
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

Smoke test:

```sh
docker run -it --rm artixlinux/artixlinux:base /bin/bash -c "echo hello world"
```

## SSH server (remote login)

Install and start:

```sh
# Arch
sudo pacman -S openssh
sudo systemctl enable sshd && sudo systemctl start sshd

# FreeBSD
sudo sysrc sshd_enable=YES && sudo service sshd start
```

Check status/logs:

```sh
sudo systemctl status sshd
sudo journalctl -u sshd
sudo grep sshd /var/log/auth.log
```

If using UFW:

```sh
sudo pacman -S ufw
sudo ufw allow ssh
sudo ufw enable
sudo ufw status
```

## VPN (Mullvad)

```sh
sudo pacman -S mullvad-vpn-daemon mullvad-vpn-openrc
```
```sh
sudo rc-update add mullvad-daemon default
sudo rc-service  mullvad-daemon start
```

```sh
mullvad status
mullvad account login <your_account_number>
mullvad relay list
mullvad relay set location se sto
mullvad connect
mullvad disconnect
```

## VS Code binary install

```sh
tar -xvzf code-stable-x64-1734606550.tar.gz
sudo mv Downloads/VSCode-linux-x64/ /opt/
sudo ln -s /opt/VSCode-linux-x64/bin/code /usr/local/bin/code
```

## Shell one-liners

USB mount, `cd -`, history substitution, and `find` tips:
[docs/shell.md](shell.md).

## X11 entry points

- Session startup / display manager / debugging: [docs/x11.md](x11.md)
- Troubleshooting X server specifics: [install/build/xlibre/README.md](../install/build/xlibre/README.md)
