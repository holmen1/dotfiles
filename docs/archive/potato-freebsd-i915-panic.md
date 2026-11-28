# Potato: FreeBSD i915 panic when starting X

Incident notes, 2026-11-25. The modesetting workaround was tested on potato
but did not produce a usable X session. The kernel panic and subsequent grey
screen remain unresolved; this is not confirmation of a kernel fix.

## Evidence

The [kernel minidump](vmcore.9) contains a boot log and panic backtrace:

- FreeBSD 15.1-RELEASE-p7, Intel Celeron N3150, Cherryview graphics
  (`8086:22b1`).
- `i915kms` initializes successfully and switches the console from `efifb` to
  `drmfb`.
- While Xorg is running, the kernel panics with a general protection fault:

  ```text
  current process = 2736 (Xorg)
  #5  vm_page_busy_acquire+0x93
  #6  lkpi_vmf_insert_pfn_prot_locked+0x105
  #7  remap_io_mapping+0xb0
  #8  vm_fault_gtt+0x4ef
  #9  linux_cdev_pager_populate+0x122
  ```

- The boot log also records an earlier general protection fault and saving
  `vmcore.8`; its backtrace is not in this dump.
- A previously inspected Xorg log showed `intel(0)` selected, but was not
  necessarily from this crash. It was subsequently replaced by the
  [log from the ShadowFB-disabled test](Xorg.0.log) below. The
  [potato profile](../../install/hosts/FreeBSD/potato.conf) installs
  `xf86-video-intel`.

## Likely cause

The stack exactly matches a report in
[freebsd/drm-kmod#526](https://github.com/freebsd/drm-kmod/issues/526).
That report attributes similar crashes in `drm-612-kmod` to registering only
the framebuffer slice of the Intel GTT aperture as fictitious VM pages.
Accessing another part of the aperture can then encounter an uninitialized
page structure. A proposed upstream fix registers the aperture more broadly:
[freebsd/drm-kmod#534](https://github.com/freebsd/drm-kmod/pull/534).

This is a strong match, not a confirmed root cause for potato: the dump alone
does not establish the installed drm-kmod version or the faulting page.
Check the installed versions and whether an upstream fix has since landed:

```sh
uname -aKU
pkg query -x '%n %v' 'drm.*kmod'
```

The previous `savecore` message alone does not establish that the earlier
panic had the same backtrace.

## Modesetting test

One reporter with the same backtrace avoided the panic by selecting Xorg's
`modesetting` driver instead of `intel`. On potato, this did not produce a
usable session, as detailed below. It still uses `i915kms` and is not a kernel
fix. Starting X may still panic, so save work first and be prepared to reboot.

1. Inspect `/usr/local/etc/X11/xorg.conf`, `/etc/X11/xorg.conf`, and their
   `xorg.conf.d` directories for existing device sections. Preserve any
   existing configuration and avoid conflicting GPU device sections.
2. Create or edit a local Xorg device snippet under
   `/usr/local/etc/X11/xorg.conf.d/`, for example `10-device.conf`:

   ```conf
   Section "Device"
       Identifier "Intel"
       Driver "modesetting"
       BusID "PCI:0:2:0"
   EndSection
   ```

3. Run `startx -- -keeptty`, then inspect `/var/log/Xorg.0.log` (or the log
   path printed by Xorg). Check for `modeset(0)` and whether the session
   remains stable. If it panics, inspect the new crash summary and restore
   the previous configuration.

To undo the test, remove the snippet if it was created solely for this test,
or restore the previous contents if an existing file was edited. Keep
`i915kms` enabled: the workaround changes the X driver, not the kernel driver.
See the upstream issue for patch status and reported results.

## Subsequent results

- With `modesetting`, Xorg detected the panel but aborted in Mesa/glamor during
  startup. The resulting client connection failure was secondary.
- Adding `Option "AccelMethod" "none"` to the device section disabled glamor
  and avoided that abort in the observed tests. One normal `startx` session
  then exited cleanly; why its client exited is unknown. Its output is in
  [startx-potato.log](startx-potato.log).
- `startx /usr/local/bin/xterm -- -keeptty` left Xorg, xinit and xterm
  running, but showed a grey screen with no visible terminal. `xwininfo`
  showed an xterm child window and `xrandr` reported normal output. A delayed
  `xsetroot -solid blue` produced no error but did not turn the screen blue
  while on X's VT 9. See [startx-xterm.log](startx-xterm.log).
- Disabling `ShadowFB` as well was confirmed in the [Xorg log](Xorg.0.log)
  but did not fix the grey screen. This was a diagnostic test, not a
  recommended workaround; remove that option to restore the previous setting.

The cause of the grey screen is unknown. Neither selecting modesetting nor
disabling acceleration establishes a kernel fix or long-term stability.

## Optional: remove the Intel X driver

The explicit `modesetting` configuration above does not require uninstalling
`xf86-video-intel`. If removing it, keep `drm-kmod` installed and `i915kms`
enabled; they provide the kernel graphics driver needed by `modesetting`.
First preview the package removal and check for unwanted dependent removals:

```sh
pkg delete -n xf86-video-intel
```

If the proposed removal is acceptable, run `sudo pkg delete xf86-video-intel`.
Remove `x11-drivers/xf86-video-intel` from the
[potato profile](../../install/hosts/FreeBSD/potato.conf) as well, or the
general installer may reinstall it. An earlier FreeBSD profile used XLibre
and its differently named `xlibre-xf86-video-intel` package.
