# FreeBSD host configuration

The FreeBSD installer selects `hosts/<hostname>.conf` using `hostname -s`
and requires `uname -s` to be `FreeBSD`. It checks the profile before
prompting; its build and install steps remain interactive.

Each profile is a trusted shell file. Copy `hosts/besk.conf` to add a host,
then edit:

- `profile_hostname` and `profile_os`: expected hostname and OS.
- `terminal` and `window_manager`: component names to build and install.
  Each needs `build-<name>.sh` and `install-<name>.sh` in
  `install/build/<name>/`; these settings do not change the X session.
- `xkb_layout`: layout passed to the XKB build script.
- `packages`: space-separated stow package names under `config/`.
- `pkg_packages`: one FreeBSD package origin per line, installed via `pkg`.
- `foreign_packages`: tracked package origins; the installer does not install them.

Keep the `# BEGIN <variable>` and `# END <variable>` markers intact so the
shared `update_config_section` function can update the package lists. To link
the profile separately:

```sh
~/repos/dotfiles/config/common/.scripts/link_config.sh \
  ~/repos/dotfiles/install/profiles/bsdinstall/hosts/besk.conf
```

On FreeBSD, update both package lists from explicitly installed packages.
The common exporter detects the OS and hostname and selects the matching
profile:

```sh
~/repos/dotfiles/config/common/.scripts/export-packages.sh
```

Export replaces the lists but preserves the rest of the profile. It groups
`FreeBSD-ports` repository entries as native, excludes `FreeBSD-base`, and
tracks other repository entries as foreign. Repository metadata does not
reliably identify ports builds; review the resulting lists before installing.
