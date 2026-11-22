# General setup

The general setup currently supports FreeBSD. It selects a trusted shell
profile at `install/hosts/<OS>/<hostname>.conf`, using `uname -s` and
`hostname -s`. Edit that profile to choose stow packages, system packages,
the terminal, window manager, and XKB layout.

Run the interactive setup or the sanity check directly:

```sh
~/repos/dotfiles/install/configure-build-install-link.sh
~/repos/dotfiles/install/sanity-check.sh
```

The sanity check checks the configured terminal and window manager, and
skips feature checks for stow packages not listed in the profile.
The configured components select build and install scripts; they do not
change the X session configuration.

To update the package lists for the current host:

```sh
~/repos/dotfiles/config/common/.scripts/export-packages.sh
```

Export updates the central profile only. The older `bsdinstall` installer
and its profiles remain independent and may differ from the central copies.
