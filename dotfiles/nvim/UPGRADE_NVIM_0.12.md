# Neovim 0.12 Upgrade Notes

This document tracks the steps, changes, and configurations modified during the upgrade to Neovim 0.12.

## Tasks
- [ ] Review current `options.lua` against Neovim 0.12 defaults.
- [ ] Check for deprecated APIs or breaking changes in Neovim 0.12.
- [ ] Update and verify plugins.
- [ ] Test language servers and formatters.

## Notes & Changes
* **Package Management**: Migrating away from `lazy.nvim` to Neovim 0.12's native package system using `vim.pack.add` and `packadd`. This removes external dependencies for plugin management.
* **File Structure**: Confirmed that Neovim 0.12 continues to use the same XDG Base Directory structure (`~/.config/nvim/` for config, `~/.local/share/nvim/` for plugins/data). The `init.lua` and `lua/` folder hierarchy remains fully supported.
* **Step 1 - Colorscheme Migration**: Removed `lazy-bootstrap` and `lazy-plugins` from `init.lua`. Configured `tokyonight` using the new built-in `vim.pack` API.
