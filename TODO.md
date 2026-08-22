# TODO index

Living index of TODOs found in code and documentation.

## Repo docs

- [README.md:186](README.md#L186) — add xv6 support.
- [install/profiles/artixinstall/README.md:182-183](install/profiles/artixinstall/README.md#L182-L183) — note about `xbacklight`/video support.
- [install/build/ghc/README.md:42-44](install/build/ghc/README.md#L42-L44) — document HLS installation.

## XMonad

- [config/xmonad/README.md:50-51](config/xmonad/README.md#L50-L51) — investigate build warnings.
- [config/xmonad/README.md:50-71](config/xmonad/README.md#L50-L71) — clarify workspace keybinding lambda.
- [install/build/xmonad/README.md:76-78](install/build/xmonad/README.md#L76-L78) — Cabal build custom xmonad.
- [install/build/xmonad/LESSONS_LEARNED.md:6-8](install/build/xmonad/LESSONS_LEARNED.md#L6-L8) — decide whether `xft` should be handled by GHC.

## dwm

- [install/build/dwm/README.md:47-50](install/build/dwm/README.md#L47-L50) — xmonad keymap and cleanup.

## Build / install scripts

- [config/common/.scripts/link_config.sh:12](config/common/.scripts/link_config.sh#L12) — improve handling of multiple packages writing to `.scripts`.
- [install/profiles/artixinstall/configure-build-install-link.sh:91](install/profiles/artixinstall/configure-build-install-link.sh#L91) — TODO script.
- [install/profiles/bsdinstall/configure-build-install-link.sh:94](install/profiles/bsdinstall/configure-build-install-link.sh#L94) — TODO script.

## Neovim

- [config/nvim/.config/nvim/lua/core/keymaps.lua:6](config/nvim/.config/nvim/lua/core/keymaps.lua#L6) — enable jumping up in terminal mode.

## Shell

- Get proper POSIX sh instead of sh -> bash

## Suggested improvements

- Make the installer/profile layout fully consistent across Artix and FreeBSD, including naming and prompt order.
- Add a short `TODO.md` workflow note in the README so humans know where to look for outstanding work.
- Document the xmonad build/install chain in one place and keep the script names aligned with that flow.
- Split recurring shell snippets and machine-specific X session choices into small shared helpers so the distro scripts stay easier to scan.
