# TODO index

Living index of TODOs found in code and documentation.

## Repo docs

- [README.md:193](README.md#L193) — add xv6 support.
- [install/profiles/artixinstall/README.md:290](install/profiles/artixinstall/README.md#L290) — note about `xbacklight`/video support.
- [install/build/ghc/README.md:42-44](install/build/ghc/README.md#L42-L44) — document HLS installation.

## XMonad

- [config/xmonad/README.md:50](config/xmonad/README.md#L50) — investigate build warnings.
- [config/xmonad/README.md:51-71](config/xmonad/README.md#L51-L71) — clarify workspace keybinding lambda.
- [install/build/xmonad/LESSONS_LEARNED.md:543](install/build/xmonad/LESSONS_LEARNED.md#L543) — decide whether `xft` should be handled by GHC.

## dwm

- [install/build/dwm/README.md:48-49](install/build/dwm/README.md#L48-L49) — xmonad keymap and cleanup.

## Build / install scripts

- [config/common/.scripts/link_config.sh:12](config/common/.scripts/link_config.sh#L12) — improve handling of multiple packages writing to `.scripts`.
- [install/profiles/artixinstall/configure-build-install-link.sh:92](install/profiles/artixinstall/configure-build-install-link.sh#L92) — TODO script.
- [install/profiles/bsdinstall/configure-build-install-link.sh:94](install/profiles/bsdinstall/configure-build-install-link.sh#L94) — TODO script.

## Neovim

- [config/nvim/.config/nvim/lua/core/keymaps.lua:6](config/nvim/.config/nvim/lua/core/keymaps.lua#L6) — enable jumping up in terminal mode.

## Shell

- Get proper POSIX sh instead of sh -> bash
Installed dash, discovered to many bashisms, on hold

## Suggested improvements

- Make the installer/profile layout fully consistent across Artix and FreeBSD, including naming and prompt order.
- Add a short `TODO.md` workflow note in the README so humans know where to look for outstanding work.
- Split recurring shell snippets and machine-specific X session choices into small shared helpers so the distro scripts stay easier to scan.
