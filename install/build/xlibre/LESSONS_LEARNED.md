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