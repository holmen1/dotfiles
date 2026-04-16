# Building XLibre from Source

This guide provides a brief, general procedure for building XLibre from source. For full details, see the [official XLibre wiki](https://github.com/X11Libre/xserver/wiki/Building-XLibre).


## Get the Source


Download the latest release tarball from the [GitHub releases page](https://github.com/X11Libre/xserver/releases) and extract it:

```sh
wget https://github.com/X11Libre/xserver/releases/download/<version>/xserver-<version>.tar.gz
mkdir -p "$XLIBRE_SRC/xserver"
tar -xzf xserver-<version>.tar.gz -C "$XLIBRE_SRC/xserver" --strip-components=1
```

Replace `<version>` with the desired release tag (e.g., `25.1`).

## Install Dependencies

- **General:** You need build tools (meson, ninja, gcc, pkg-config) and X11 libraries. See the [wiki](https://github.com/X11Libre/xserver/wiki/Building-XLibre#dependencies-for-xlibre-xserver-on-debiandevuan) for distro-specific lists.
- **Arch Linux:** Use system packages, e.g. `base-devel`, `meson`, `ninja`, `libx11`, etc. (see [xorgproto](https://archlinux.org/packages/extra/any/xorgproto/)).
- **FreeBSD:** Use `pkg install` for dependencies, but some may need to be built from ports. Details may differ; consult the wiki and FreeBSD ports tree.

## Build and Install

```sh
cd "$XLIBRE_SRC/xserver"
meson setup --prefix "$XLIBRE_PREFIX" "$XLIBRE_BUILD" --buildtype debugoptimized
ninja -C "$XLIBRE_BUILD"
sudo ninja -C "$XLIBRE_BUILD" install
```

## Drivers

Build and install input/video drivers as needed. See [wiki section](https://github.com/X11Libre/xserver/wiki/Building-XLibre#building-the-xlibre-drivers). Most use either Meson or GNU autotools.

## Debugging

- Use `--buildtype debug` or `debugoptimized` with Meson for debug builds.
- Logs are in `/var/log/` or as specified by your config.
- Run with `-verbose` for more output.

## Testing

Create a test script:
```sh
#!/bin/sh
./bin/X :1 vt8 &
_pid=$!
sleep 10 && kill $_pid
```
Make it executable and run. You should see a blank screen for 10 seconds if successful.

## Notes

- On **Arch**, prefer the official packages unless you need to patch or test upstream.
- On **FreeBSD**, some dependencies or build flags may differ; check the wiki and ports.
- For more details, troubleshooting, and advanced options, see the [official XLibre wiki](https://github.com/X11Libre/xserver/wiki/Building-XLibre).

