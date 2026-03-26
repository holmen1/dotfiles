# Neovim 0.12 Upgrade Notes

This document tracks the steps, changes, and configurations modified during the upgrade to Neovim 0.12.

## Tasks
- [ ] Review current `options.lua` against Neovim 0.12 defaults.
- [ ] Check for deprecated APIs or breaking changes in Neovim 0.12.
- [ ] Update and verify plugins.
- [ ] Test language servers and formatters.

## Notes & Changes
* **Package Management**: Migrating away from `lazy.nvim` to Neovim 0.12's native package system using `vim.pack.add` and `packadd`. This removes external dependencies for plugin management. Installation happens concurrently but natively **blocks** until finished before continuing the `init.lua` execution.
* **File Structure**: Confirmed that Neovim 0.12 continues to use the same XDG Base Directory structure (`~/.config/nvim/` for config, `~/.local/share/nvim/` for plugins/data). The `init.lua` and `lua/` folder hierarchy remains fully supported.

## Migration Steps
* **Step 1 - Colorscheme Migration**: Removed `lazy-bootstrap` and `lazy-plugins` from `init.lua`. Configured `tokyonight` using the new built-in `vim.pack` API. Moved config nicely into `lua/plugins/theme.lua`.
* **Step 2 - Treesitter Migration**: Converted `nvim-treesitter` config to native format in `lua/plugins/treesitter.lua`. Loaded it natively in `init.lua`. 
    * *Note*: `:TSUpdate` will need to be run manually the very first time. 
    * *Dependency Check*: Ensure `tree-sitter` (version > 26.1) and a C compiler like `gcc` are installed on the host OS. Note that native package managers (like `pacman` on Arch) might lag (e.g., version 25). To get `> 26.1`, it is recommended to install the CLI via `npm install -g tree-sitter-cli` or `cargo install --locked tree-sitter-cli` or fetching a binary directly. Without an external plugin manager handling build hooks in the background, missing OS dependencies when compiling parsers become more apparent.
    * *Breaking API Change*: The newest main branch of `nvim-treesitter` (which `vim.pack` fetches by default) removed `require('nvim-treesitter.configs')`. You must now use `require('nvim-treesitter').setup{}` instead.

## Helpful Commands
* **Run Headless Checkhealth**: If debugging an environment purely from the terminal without opening UI, run `nvim --headless -c "checkhealth" -c "w! health.log" -c "qa"` to pipe output to `health.log`.
