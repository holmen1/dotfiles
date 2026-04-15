# LESSONS LEARNED

## XLibre is Now Available as a Binary Package

XLibre landed in the FreeBSD ports tree in December 2025 and binary packages are now available. **Prefer `pkg` over ports** — it keeps everything consistently versioned and `pkg upgrade` handles it automatically.

**Install via pkg (recommended):**
```bash
sudo pkg install xlibre-minimal xlibre-xf86-video-intel
```

**Why ports caused breakage:** When XLibre was port-built but its dependencies (e.g., `xlibre-xf86-video-vesa`, `xlibre-xf86-input-libinput`) were upgraded as binary packages via `pkg upgrade`, the version skew between the port-built server and the upgraded binary dependencies caused X to fail with `signal 11 (segmentation fault)` or fall back to `scfb`. Switching fully to `pkg` resolved it immediately.

**General rule:** Never mix port-built and pkg-installed versions of tightly coupled packages (server + drivers). Use one or the other consistently.

---

## pkg upgrade Can Break Mesa (crocus_dri / glamor crash)

**Symptom:** After `pkg upgrade`, `startx` freezes. `Xorg.0.log` shows a crash inside `crocus_dri.so` during `glamor_init`:

```
7: /usr/local/lib/dri/crocus_dri.so
...
19: glamor_init
20: modesetting_drv.so
Fatal server error: Caught signal 6 (Abort trap). Server aborting
```

**Cause:** A regression in the `mesa-dri` package for Braswell/Gen8 Intel GPUs (device `8086:22b1`). The `modesetting` driver's default acceleration method (`glamor`) uses OpenGL/EGL via Mesa, and the upgraded `crocus_dri.so` crashes on initialization.

**Workaround (immediate):** Disable glamor acceleration in `/usr/local/etc/X11/xorg.conf.d/20-modesetting.conf`:

```
Section "Device"
    Identifier  "Intel Graphics"
    Driver      "modesetting"
    Option      "AccelMethod" "none"
EndSection
```

This bypasses the Mesa/OpenGL stack entirely. Software rendering is slower but perfectly usable for XMonad with terminals.

**Alternative fix (tested working):** If the above config causes issues, remove the entire `/usr/local/share/X11/xorg.conf.d/` directory (or move to .bak). This forces X to autoconfigure without any device-specific configs, allowing `modesetting` with glamor to work properly on Braswell GPUs. However, `startx` with the built-in config still crashes due to the Mesa regression— the built-in includes intel in the ServerLayout, which triggers the bug even though intel is removed. Workaround: Create `/usr/local/etc/X11/xorg.conf` with a minimal modesetting config to override the built-in.

**Proper fix:** Wait for a Mesa package update in the FreeBSD repo, then remove the `AccelMethod` line or re-enable configs to re-enable hardware acceleration. Check with:

```bash
pkg info mesa-dri
```

**Diagnosis steps used:**

**Diagnosis steps used:**
1. `X -probeonly` — safe way to test X without launching a window manager
2. `startx ~/.xinitrc.test` (containing only `exec xterm`) — isolates X from XMonad
3. `grep -E 'modesetting|signal|EE|Fatal' /var/log/Xorg.0.log` — quick log scan


---

## 2026-04-15: Xorg Ignores xorg.conf — Deep Dive

**Symptom:** Xorg uses "default built-in configuration" even when `/usr/local/etc/X11/xorg.conf` exists and is correct. Log shows:

```
(==) Using default built-in configuration (39 lines)
```

**Key findings:**

- Xorg config file search order on FreeBSD:
    1. `/usr/local/etc/X11/xorg.conf` (monolithic, overrides all)
    2. `/usr/local/etc/X11/xorg.conf.d/*.conf` (user/admin snippets)
    3. `/usr/local/share/X11/xorg.conf.d/*.conf` (package defaults)
    4. If none found, falls back to built-in config
- If Xorg is started with `-config` or `-configdir`, it will only use the specified file/dir.
- Permissions or typos in the config path will cause Xorg to skip the file silently and use built-in config.
- `startx` and `xinit` do **not** override config file usage unless arguments are passed (see script analysis).
- Always check `/var/log/Xorg.0.log` for lines like `Using config file:` or `Using default built-in configuration` to confirm which config is loaded.

**Explicit config test:**

To force Xorg to use a config file and get verbose logging:

```sh
xinit ~/.xinitrc -- /usr/local/bin/X -config /usr/local/etc/X11/xorg.conf -logverbose 6
```

**Troubleshooting checklist:**

- Confirm `/usr/local/etc/X11/xorg.conf` exists, is readable, and not empty.
- Check for typos in the filename or path.
- Check for `-config` or `-configdir` in Xorg command line (via `ps aux | grep Xorg`).
- Try launching Xorg directly with `-config` as above.
- Always check `/var/log/Xorg.0.log` for config file usage.

**If all else fails:**
- There may be a deeper issue with Xorg's config file detection or a bug in the X server version.
- Consider downgrading Xorg or related packages, or waiting for an upstream fix.