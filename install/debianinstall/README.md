# debianinstall

Profile-based install script for automating dotfiles, package installation, and system configuration on Debian.
The base desktop is **Xfce** (installed initially), with **xmonad** as the primary WM.
Debian-specific monitor scripts live in `debianinstall/scripts/` and hook into Xfce/NetworkManager rather than the Arch/iwd equivalents.

## Automated Install (Recommended)

1. **Ensure your user is in the sudo group:**

   On Debian, `su` alone keeps your current environment. Use `su --login` (or `su -`) to get a proper root login shell with root's `PATH` — otherwise commands like `adduser` may not be found.
   ```sh
   su --login
   root@host:~# adduser <youruser> sudo
   ```
   Log out and back in for the group change to take effect.
2. **Clone your dotfiles repo:**
   ```sh
   mkdir -p ~/repos
   git clone git@github.com:holmen1/dotfiles.git ~/repos/dotfiles
   cd ~/repos/dotfiles/install/debianinstall
   ```
3. **Run the install script:**
   ```sh
   ./configure_build_install_link.sh
   ```
   - Auto-detects profile by hostname; prompts for each step: git config, SSH key, package install, build xmonad/st/xkb, link dotfiles, enable services, run tests.
   - Packages: `packages/<profile>/pkglist.txt`
   - Dotfile links: `links/<profile>/links.config` (uses `systemd-debian` stow package, not the generic `systemd`)

## Manual Steps (if needed)

- **Disable CDROM in apt sources:**
  Edit `/etc/apt/sources.list` and comment out any `cdrom:` lines.

- **WiFi management (nmcli):**
  - Show status: `nmcli general status`
  - List devices: `nmcli device status`
  - List WiFi networks in range: `nmcli device wifi list`
  - Connect new network (creates saved profile): `nmcli device wifi connect <SSID> password <password>`
  - List saved profiles: `nmcli connection show`
  - Connect using saved profile: `nmcli connection up id "<SSID>"`
  - Delete saved profile: `nmcli connection delete "<SSID>"`
  - Toggle WiFi: `nmcli radio wifi on|off`

  > dmenu Scan only works for SSIDs with a saved profile. Use `nmcli device wifi connect` once to create the profile, then Scan works for future connects.

- **Cleanup:**
  ```sh
  sudo apt autoremove
  sudo apt clean
  ```

## Package Management

Run from `install/debianinstall/`:

- Export currently installed packages:
  ```sh
  scripts/export-apt.sh packages/<profile>
  ```
- Install from list manually:
  ```sh
  scripts/install-apt.sh packages/<profile>/pkglist.txt
  ```

## Debian-specific Scripts (`debianinstall/scripts/`)

| Script | Backend | Purpose |
|---|---|---|
| `monitor-wifi.sh` | `nmcli` (NetworkManager) | WiFi state notifications + connect/scan helpers |
| `monitor-battery.sh` | `/sys/class/power_supply/` | Battery level notifications |
| `install-apt.sh` | `apt-get` | Install packages from pkglist |
| `export-apt.sh` | `apt-mark` | Export manually installed packages to pkglist |

The monitor scripts replace the repo-level `scripts/monitor-*.sh` which target Arch/iwd/FreeBSD.
Both are wired into the `system-monitor` systemd user timer via the `systemd-debian` stow package (`dotfiles/systemd-debian/`), which runs checks every 2 minutes.

## Build Neovim from Source

Debian's packaged neovim lags behind. Build v0.12 from source:

```sh
../../build/neovim/build-neovim.sh
```

This clones the neovim repo, checks out the `stable` tag, builds with `cmake`, and installs to `/usr/local`. Requires `cmake`, `make`, `clang`, and `libluajit` dev packages (included in `pkglist.txt`).

## Xmonad, dmenu, etc.
- Built/installed via the install script prompts. See `install/build/` for build scripts.
- `xmonad.hs` reads `$TERMINAL` and `$BROWSER` from the environment (set in `.xinitrc` or session script).

---

## Xmonad session in LightDM (with Xfce)

To add xmonad as a session option alongside Xfce in LightDM:

1. **Create a session file** `/usr/share/xsessions/xmonad.desktop`:
   ```ini
   [Desktop Entry]
   Name=Xmonad
   Exec=/home/holmen1/.config/xmonad-session-rc
   Type=Application
   ```

2. **Session script** `~/.config/xmonad-session-rc`:
   ```sh
   #!/bin/sh
   exec ~/.xinitrc
   ```

3. **Select Xmonad at login:** log out, choose "Xmonad" from the session menu.

## Lessons Learned

- `.xinitrc` must be executable (`chmod +x ~/.xinitrc`)


