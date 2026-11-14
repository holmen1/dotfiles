# Shell commands

Handy shell one-liners. Topic-specific procedures (install, networking, X11
debugging, builds) stay in their own pages; this page is the reusable bits.

## Mount and unmount

### USB

```sh
lsblk
sudo mkdir -p /mnt/usb
sudo mount /dev/sdX1 /mnt/usb
sudo umount /mnt/usb
```

Replace `sdX1` with the partition shown by `lsblk`.

### Recursive unmount

Used when leaving a chroot or rescue mount tree (see the
[Artix ISO rescue path](artix-post-install.md#iso-rescue-path-if-the-boot-path-fails)):

```sh
umount -R /mnt
```

## Write a disk image

Identify the target first. Writing to the wrong device wipes it.

```sh
dd if=image.img of=/dev/da0 bs=1M conv=sync
```

## Console TTY

- `Ctrl+Alt+F2` through `Ctrl+Alt+F6` — switch to a TTY
- `Ctrl+Alt+F7` — return to the X session

Once at a TTY:

```sh
ps aux | grep X           # check if X is running
pkill -9 X                # kill a stuck X server
startx                    # restart the X session
```

## Directory navigation

```sh
cd -                  # toggle back to the previous directory
cd "$OLDPWD" && pwd   # same, and print where you landed
```

`shopt -s autocd` is set in `.bashrc`, so a bare directory name changes into it.

## History

Rerun the previous command with the first `old` replaced by `new`:

```sh
^old^new
```

`.bashrc` also sets `HISTCONTROL=ignoreboth:erasedups` (drop duplicates and
commands that start with a space) and vi-mode (`set -o vi`):

| Key | Action |
|---|---|
| Esc | Command mode |
| i | Insert mode |
| / | Search history |
| n / N | Next / previous match |
| k / j | Previous / next command |
| Up / Down | History search backward / forward |

## Search

```sh
find /path/to/search -name "*.txt" -exec grep "search term" {} \;
```

## Terminal colors

Preview 256-color values used by the palette
([colors README](../config/colors/README.md)):

```sh
for i in 179 173 185 187; do printf '\e[38;5;%dm %3d ██\e[0m\n' "$i" "$i"; done
```

## See also

Command blocks that belong with a procedure, not here:

- X11 process and debug commands: [x11.md](x11.md)
- XKB keymap checks: [config/xkb/README.md](../config/xkb/README.md)
- OpenRC service commands: [networking.md](networking.md)
- Build fetch and compile patterns: [build.md](build.md)
- Post-install tasks: [Artix](artix-post-install.md) or [FreeBSD](bsd-post-install.md)
