# build/xkb

Compiles the XKB keymap.

Keymap behavior, level-3 mappings, and debugging:
[config/xkb/README.md](../../../config/xkb/README.md)

## Files

| Path              | Purpose                                              |
|-------------------|------------------------------------------------------|
| `build-xkb.sh`    | Compiles keymap into `~/.cache/custom-keymap.xkb` |
| `symbols/local`   | XKB partial symbols — num-layer digit assignments    |

## Usage

```sh
install/build/xkb/build-xkb.sh <se|us>
```

Re-run whenever `symbols/local` changes.

## Dependencies

```sh
sudo pacman -S xorg-setxkbmap xorg-xkbcomp
```

## How it works

1. `setxkbmap -print` emits the base keymap description to stdout
2. `sed` appends `+local(<layout>)` to the `xkb _symbols` include line
3. `xkbcomp` compiles the result into `~/.cache/custom-keymap.xkb`
