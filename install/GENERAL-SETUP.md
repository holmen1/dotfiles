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
skips feature checks for stow packages not listed in the profile. The shared
X session also uses the configured terminal, window manager and XKB layout.
Re-link the profile after updating to migrate an existing distro-specific
`.xinitrc` symlink.

On FreeBSD, `install/pre-cbil.sh` can install the profile's `pkg_pre` list.
The general setup's package prompt installs `pkg_packages` as before. To
install a particular group separately, use:

```sh
~/repos/dotfiles/config/common/.scripts/import-packages.sh --pre
```

Replace `--pre` with `--packages` or `--foreign` to select `pkg_packages` or
`foreign_packages`; omitting the option selects `--packages`. A selected
variable must exist in the profile, though an empty list is allowed. Foreign
origins are installed with `pkg` using the host's configured repositories;
review them first because export classifies them using repository metadata.
Export replaces `pkg_packages` and `foreign_packages`, not `pkg_pre`.
Import selects the current FreeBSD host profile automatically. Artix import
is not implemented yet.

The Artix `x1` profile uses Xorg, `st`, XMonad and the Swedish layout. Its
package helper installs native packages using `pacman`; foreign packages are
tracked but not installed by this setup. Link dotfiles before
building XMonad. The optional GHC and XMonad library prompts precede the
XMonad build: the profile specifies GHC 9.12.4, XMonad 0.18.1 and
xmonad-contrib 0.18.2. The GHC installer uses an x86_64 Debian 12 binary
distribution, which must be compatible with the machine. Building the XKB
keymap requires an active X display.

The Artix `besk` profile is for the Framework laptop and uses the US layout;
`gadsden` uses the Swedish layout and retains its legacy XLibre package list.
Both use the same XMonad and GHC versions as `x1`.

The `services` field lists services to enable at boot. Artix profiles enable
`iwd`, `dbus` and `elogind` when prompted. FreeBSD profiles start with an
empty list; add the desired rc service names to enable them through setup.

The optional system-command link step uses
`config/common/.scripts/link-system-commands.sh` for `/usr/local/bin`.

Direct XMonad build and install scripts also use the matching host profile;
without one they retain their standalone version defaults. The library and
binary builds require the configured GHC version. `xmonad --recompile` stages a
binary; run `install/build/xmonad/install-custom-xmonad.sh` to deploy it.

To update the package lists for the current host:

```sh
~/repos/dotfiles/config/common/.scripts/export-packages.sh
```

Export updates the central profile only. On Artix it records explicitly
installed native and foreign packages. The older `bsdinstall` installer and
its profiles remain independent and may differ from the central copies.
