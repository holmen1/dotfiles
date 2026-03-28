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
* **Step 2 - Which-Key Migration**: Unpacked `which-key` from Lazy array bindings. Because native plugin installs don't have dependency tree handling natively, it is crucial to initialize `which-key` in `init.lua` *before* the other UI plugins (like Telescope) so it can hijack the `timeoutlen` and intercept leader mapping pauses properly, otherwise slow typing drops Neovim into Insert mode when pressing `<Space>sf`.
* **Step 3 - Treesitter Migration**: Converted `nvim-treesitter` config to native format in `lua/plugins/treesitter.lua`. Loaded it natively in `init.lua`. 
    * *Note*: `:TSUpdate` will need to be run manually the very first time. 
    * *Dependency Check*: Ensure `tree-sitter` (version > 26.1) and a C compiler like `gcc` are installed on the host OS. Note that native package managers (like `pacman` on Arch) might lag (e.g., version 25). To get `> 26.1`, it is recommended to install the CLI via `npm install -g tree-sitter-cli` or `cargo install --locked tree-sitter-cli` or fetching a binary directly. Without an external plugin manager handling build hooks in the background, missing OS spikes when compiling parsers become more apparent.
    * *Breaking API Change*: The newest main branch of `nvim-treesitter` (which `vim.pack` fetches by default) removed `require('nvim-treesitter.configs')`. You must now use `require('nvim-treesitter').setup{}` instead.
* **Step 3 - Telescope Migration**: Removed lazy dependency structures in `lua/plugins/telescope.lua`. Converted `telescope-fzf-native` pre-build hooks to the new native `vim.api.nvim_create_autocmd('PackChanged')` firing heavily optimized background `vim.system({ 'make' })` execution loops in `init.lua`. Added `plenary` and all extensions separately to `init.lua` list.
* **Step 4 - LSP Migration**: Fully maintained the "half-manual" approach without Mason. Converted `lspconfig.lua` to remove the lazy wrapper. Explicitly added `fidget.nvim` and `lazydev.nvim` to `vim.pack.add` alongside `nvim-lspconfig`. Confirmed that Neovim 0.12 officially uses `nvim-lspconfig` under the hood strictly as a *default configurations source* for the builtin `vim.lsp.config('server')` and `vim.lsp.enable()` functions.
* **Step 5 - Gitsigns Migration & Lazy Cleanup**: Completely transitioned `gitsigns.nvim` off of `lazy.nvim`, validating the final port. Cleaned up all leftover lazy plugin cache directories (`~/.local/share/nvim/lazy`) and removed any remaining `lazy.nvim` dependencies scattered in the config since the native 0.12 package manager seamlessly replaced it all.

## Helpful Commands
* **Run Headless Checkhealth**: If debugging an environment purely from the terminal without opening UI, run `nvim --headless -c "checkhealth" -c "w! health.log" -c "qa"` to pipe output to `health.log`.

## Platform Notes
* **macOS**: `brew install neovim` delivers 0.12 stable.
* **Arch Linux**: `pacman -S neovim` delivers 0.12 stable.
* **FreeBSD**: Neither `pkg` nor ports have 0.12 yet (ports only at 0.11.6 as of March 2026). Must build from source — see [`install/build/neovim/build-neovim.sh`](../../install/build/neovim/build-neovim.sh).
