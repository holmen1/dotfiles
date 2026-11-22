# General setup

The general setup supports FreeBSD and Artix Linux. It selects a trusted shell
profile at `install/hosts/<OS>/<hostname>.conf`, using `uname -s`,
`hostname -s` and, on Linux, `/etc/os-release`. Other Linux distributions and
hosts without a matching profile are not supported. Edit the profile to choose
stow packages, system packages, terminal, window manager and XKB layout.

Run the interactive setup or the sanity check directly:

```sh
~/repos/dotfiles/install/configure-build-install-link.sh
~/repos/dotfiles/install/sanity-check.sh
```

The sanity check checks the configured terminal and window manager, and
skips feature checks for stow packages not listed in the profile.
The configured components select build and install scripts; they do not
change the X session configuration.

The Artix `x1` profile uses Xorg, `st`, XMonad and the Swedish layout. It
installs native packages using `yay` when available, otherwise `pacman`; foreign
packages are tracked but not installed by this setup. Link dotfiles before
building XMonad. The optional GHC and XMonad library prompts precede the
XMonad build: the profile specifies GHC 9.12.4, XMonad 0.18.1 and
xmonad-contrib 0.18.2. The GHC installer uses an x86_64 Debian 12 binary
distribution, which must be compatible with the machine. Building the XKB
keymap requires an active X display.

Direct XMonad build and install scripts also use the matching host profile;
without one they retain their standalone version defaults. The library and
binary builds require the configured GHC version. `xmonad --recompile` stages a
binary; run `install/build/xmonad/install-custom-xmonad.sh` to deploy it.

To update the package lists for the current host:

```sh
~/repos/dotfiles/config/common/.scripts/export-packages.sh
```

Export updates the central profile only. On Artix it records explicitly
installed native and foreign packages. The older `bsdinstall` and
`artixinstall` installers and profiles remain independent and may differ
from the central copies; the legacy Artix `x1` package list is stale.
