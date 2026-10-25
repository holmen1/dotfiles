# Hardware notes

Machine-specific quirks and confirmed workarounds.

## Framework AMD (`besk`)

### Display: apps only redraw on touchpad movement

If X applications draw only after touchpad movement, the shell and keyboard
are not blocked. Do **not** change input groups, input drivers, the kernel, or
session daemons to address this symptom.

Confirmed workaround: `amdgpu.dcdebugmask=0x610` — disables PSR, PSR selective
update, and Panel Replay.

1. Add it to the existing `GRUB_CMDLINE_LINUX_DEFAULT`
2. Regenerate GRUB config (`grub-mkconfig -o /boot/grub/grub.cfg`)
3. Reboot

Do not keep `0x10` — it disables only PSR and did not fix the issue. The full
mask has a small idle-power cost; do not narrow it without testing.

Verify: `grep amdgpu /proc/cmdline`; the corresponding
`/sys/module/amdgpu/parameters/dcdebugmask` value is `1552`.

### RDSEED32 boot message

`RDSEED32 is broken. Disabling the corresponding CPUID bit.` is a mitigation,
not a failed boot. Some Zen 5 CPUs can return invalid 16-bit or 32-bit
`RDSEED` values; Linux clears the affected capability until the BIOS supplies
the required microcode.

Fix: update the Framework BIOS, reboot, check the running revision with
`grep microcode /proc/cpuinfo`. Do **not** hide the message with `loglevel`
or add `clearcpuid=rdseed` — the kernel has already applied the safe
workaround.

X11 input configuration (e.g. touchpad tap-to-click) lives in
[docs/x11.md](x11.md#input-devices).
