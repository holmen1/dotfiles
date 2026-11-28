# General setup

The general setup supports FreeBSD and Artix Linux. It selects a trusted shell
profile at `install/hosts/<OS>/<hostname>.conf`, using `uname -s`,
`hostname -s` and, on Linux, `/etc/os-release`. Other Linux distributions and
hosts without a matching profile are not supported. Edit the profile to choose
stow packages, system packages, terminal, window manager and XKB layout.

Clone the repository over HTTPS while still in the TTY, then run the pre-setup
first, followed by the main setup and sanity check:

```sh
mkdir -p ~/repos
git clone https://github.com/holmen1/dotfiles.git ~/repos/dotfiles
~/repos/dotfiles/install/scripts/pre-configure-build-install-link.sh
git -C ~/repos/dotfiles remote set-url origin git@github.com:holmen1/dotfiles.git
git -C ~/repos/dotfiles ls-remote origin HEAD
~/repos/dotfiles/config/common/.scripts/configure-build-install-link.sh
~/repos/dotfiles/config/common/.scripts/sanity-check.sh
```

The pre-setup configures Git and SSH, installs the initial package list, and
builds/installs the terminal. It also prompts about GHC and XMonad libraries
on every supported host. On FreeBSD, GHC is installed through `pkg`; on
Artix, the configured GHC installer is used. The main setup prompts to install
the package and foreign-package lists, then offers one shared dotfile-linking
step before building/installing the window manager and XKB keymap. Add the
generated `~/.ssh/id_ed25519.pub` key to GitHub before the pre-setup's final
check; it verifies permissions and confirms GitHub recognizes the key. After
pre-setup succeeds, the commands above switch the HTTPS clone's remote to SSH.
Pre-setup also offers to create a temporary `~/.xinitrc` running `xterm` if
none exists. This supports an optional X test before the main setup, provided
`xterm` and the X stack are installed; the dotfile-link step replaces it with
the configured session startup.

The sanity check checks the configured terminal and window manager, and
skips feature checks for stow packages not listed in the profile. The shared
X session also uses the configured terminal, window manager and XKB layout.
Re-link the profile after updating to migrate an existing distro-specific
`.xinitrc` symlink.

The pre-setup can install FreeBSD's `pkg_pre` list; Artix has no `pkg_pre`
group. The main setup prompts for `pkg_packages` and `foreign_packages` on
both systems. To install a particular group separately, use:

```sh
~/repos/dotfiles/config/common/.scripts/import-packages.sh --pre
```

Replace `--pre` with `--packages` or `--foreign` to select `pkg_packages` or
`foreign_packages`; omitting the option selects `--packages`. A selected
variable must exist in the profile; an empty list is skipped. FreeBSD
installs package origins with `pkg`; Artix uses `pacman` for both lists.
Artix foreign packages must be available in a configured repository; the
importer does not use an AUR helper. Export replaces `pkg_packages` and
`foreign_packages`, not `pkg_pre`. Import selects the current supported host
profile automatically.

The general import/export entry points and runtime Wi-Fi/battery dispatchers
are Stow-linked from `config/common/.scripts`. They select implementations
under `install/hosts/Linux/scripts` or `install/hosts/FreeBSD/scripts`;
shared host setup helpers live under `install/hosts/scripts`.

The Artix `x1` profile uses Xorg, `st`, XMonad and the Swedish layout. Its
package importer uses `pacman` for both native and foreign package lists.
Link dotfiles before
building XMonad. The optional GHC and XMonad library prompts in the pre-setup
precede the XMonad build: the profile specifies GHC 9.12.4, XMonad 0.18.1 and
xmonad-contrib 0.18.2. The Artix GHC installer uses an x86_64 Debian 12
binary distribution, which must be compatible with the machine. Building the
XKB keymap requires an active X display.

The Artix `besk` profile is for the Framework laptop and uses the US layout;
`gadsden` uses the Swedish layout and retains its legacy XLibre package list.
Both use the same XMonad and GHC versions as `x1`.

The `services` field lists services to enable at boot. Artix profiles enable
`iwd`, `dbus` and `elogind` when prompted. FreeBSD profiles start with an
empty list; add the desired rc service names to enable them through setup.

The optional system-command link step uses
`install/hosts/scripts/link-system-commands.sh` for `/usr/local/bin`.

Direct XMonad build and install scripts also use the matching host profile;
without one they retain their standalone version defaults. The library and
binary builds require the configured GHC version. `xmonad --recompile` stages a
binary; run `install/build/xmonad/install-custom-xmonad.sh` to deploy it.

To update the package lists for the current host:

```sh
~/repos/dotfiles/config/common/.scripts/export-packages.sh
```

Export updates the central profile only. On Artix it records explicitly
installed native and foreign packages. FreeBSD setup now uses the same
host-profile-driven installer as Artix; current FreeBSD profiles live in
`install/hosts/FreeBSD/`.
