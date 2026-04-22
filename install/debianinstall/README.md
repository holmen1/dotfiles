# debianinstall

This setup uses a profile-based install script for automating dotfiles, package installation, and system configuration on Debian systems.

## Automated Install (Recommended)

1. **Ensure your user is in the sudo group:**
   ```sh
   su --login
   root@host:~# adduser <youruser> sudo
   ```
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
   - The script will auto-detect your profile (by hostname) and prompt for each step: git config, SSH key, package install, build, link, enable services, and run tests.
   - Packages are read from `packages/<profile>/pkglist.txt`.
   - Dotfile links are read from `links/<profile>/links.config`.

## Manual Steps (if needed)

- **Disable CDROM in apt sources:**
  Edit `/etc/apt/sources.list` and comment out any `cdrom:` lines.

- **WiFi management (nmcli):**
  - Show status: `nmcli general status`
  - List devices: `nmcli device status`
  - List WiFi: `nmcli device wifi list`
  - Connect: `nmcli device wifi connect <SSID> password <password>`
  - Saved network: `nmcli connection up "Network Name"`
  - Toggle WiFi: `nmcli radio wifi on|off`

- **Cleanup:**
  ```sh
  sudo apt autoremove
  sudo apt clean
  sudo flatpak uninstall --unused
  ```

## Package Management
- Packages are managed via `pkglist.txt` per profile. To update your list:
  ```sh
  ./scripts/export-apt.sh install/debianinstall/packages/<profile>
  ```
- To install from a list manually:
  ```sh
  ./scripts/install-apt.sh install/debianinstall/packages/<profile>/pkglist.txt
  ```

## Xmonad, dmenu, etc.
- Xmonad and related tools are built/installed via the script. See prompts during install.
- For custom builds, see the `install/build/` directory.

---


## Xmonad session in LightDM (with Xfce)
To add xmonad as a session option in LightDM:

1. **Create a session file:**
  Create `/usr/share/xsessions/xmonad.desktop` with:
  ```ini
  [Desktop Entry]
  Name=Xmonad
  Exec=/home/holmen1/.config/xmonad-session-rc
  Type=Application
  ```

2. **Ensure your session script launches xmonad:**
  Your `xmonad-session-rc` should exec your `.xinitrc`e.g.:
  ```sh
  #!/bin/sh
  exec ~/.xinitrc
  ```

3. **Select Xmonad at login:**
  Log out, then choose "Xmonad" from the session menu in LightDM.

For more, see your `bin/xmonad-session-rc` and `.xinitrc` for customizations.

## Lessons Learned

Needed make .xinitrc executable

### st
install nerdfonts .ttf in
```bash
~/.local/share/fonts/
```

