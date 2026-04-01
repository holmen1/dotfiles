# build/xkb

Compiles the XKB keymap used by `dotfiles/xkb`.

## Files

| Path              | Purpose                                              |
|-------------------|------------------------------------------------------|
| `build-xkb.sh`    | Compiles keymap into `dotfiles/xkb/.config/xkb/keymap.xkb` |
| `symbols/local`   | XKB partial symbols — num-layer digit assignments    |

## Usage

```sh
install/build/xkb/build-xkb.sh
```

Re-run whenever `symbols/local` changes.

## Dependencies

```sh
sudo pacman -S xorg-setxkbmap xorg-xkbcomp
```

## How it works

1. `setxkbmap -print` emits the base keymap description to stdout
2. `sed` appends `+local(numpad)` to the `xkb_symbols` include line
3. `xkbcomp` compiles the result into `dotfiles/xkb/.config/xkb/keymap.xkb`
