# slim-vim

A lean Neovim config built around a small set of stable plugins and system-installed tools.

## Layout

- `lua/core/options.lua` — editor options
- `lua/core/keymaps.lua` — global keymaps
- `lua/core/autocmds.lua` — autosave and yank highlight
- `lua/plugins/init.lua` — lazy.nvim plugin list
- `lua/plugins/*.lua` — plugin setup
- `sitiom/nvim-numbertoggle` — automatic relative/absolute line numbers
- `lua/custom/terminal.lua` — terminal toggle

## Package strategy

- **Plugins:** `lazy.nvim`
- **LSP/runtime tools:** system package manager

No Mason-style tool manager; install servers and CLIs where the OS expects them.

## What it does

- Telescope for file, buffer, grep, and keymap search
- Treesitter for syntax highlighting
- Native LSP via `nvim-lspconfig`
- Gitsigns for git hunks and inline diffing
- Mini for buffer tabline, statusline, and textobjects
- Simple terminal toggle on `<leader>tt`
- Autosave on edits and insert leave

## Main keymaps

### Global

- `<C-j>` / `<C-k>` — move between windows
- `<C-A-j/k>` — resize splits

### Telescope

- `<leader>sf` — find files
- `<leader>sg` — live grep
- `<leader>sw` — grep word under cursor
- `<leader>sr` — resume search
- `<leader>sk` — search keymaps
- `<leader><leader>` — buffers
- `<leader>sc` — git commits for current buffer
- `:Telescope fidget` — notification history

### LSP

- `K` — hover documentation
- `gd` — definition
- `gD` — declaration
- `grr` — references
- `gri` — implementations
- `grn` — rename
- `<leader>q` — diagnostic list
- `<leader>th` — toggle inlay hints

### Git

- `]c` / `[c` — next / previous hunk
- `<leader>gd` — preview hunk
- `<leader>gr` — reset hunk
- `<leader>gR` — reset buffer
- `<leader>ga` — stage hunk

### Terminal

- `<leader>tt` — toggle bottom terminal

**After opening the terminal:**

1. Neovim starts you in terminal-insert mode, so you can type commands right away.
2. Press `<Esc>` to leave terminal insert mode and go back to terminal normal mode.
3. Use `<C-k>` / `<C-j>` to move between the terminal split and the rest of Neovim.
4. Press `i` to type in the terminal again.
5. Press `<leader>tt` again to close the terminal split.

## Notes

- LSP servers are expected to be available on `PATH`
- Keymap may timeout if typing to slow, set `vim.opt.timeoutlen = 1500`
- Adding plugins: [ADDING_PLUGINS.md](ADDING_PLUGINS.md)
- Past incidents: [LESSONS_LEARNED.md](LESSONS_LEARNED.md)
- Generic Vim reference: [docs/vim-quickref.md](../../docs/vim-quickref.md)

## Health checks

If the config behaves strangely, use:

```bash
nvim --headless -c "checkhealth" -c "qa"
```

## LSP troubleshooting

This config bypasses Mason entirely. LSP servers are installed via the system package manager.

### Install LSP servers

| Server | Artix | FreeBSD |
|--------|------|---------|
| clangd | `clang` | `llvm` |
| lua_ls | `lua-language-server` | `lua-language-server` |
| hls | `extra/haskell-language-server` | `hs-haskell-language-server` |
| bash | `extra/bash-language-server`, `extra/shfmt` | `hs-ShellCheck`, `shfmt` (not tested) |

### Check status

```vim
:LspLog
:checkhealth vim.lsp
```

### Common issues

**"No LSP implementation found"**
- Server not installed
- Server not in `PATH`
- Not a project: HLS needs `*.cabal`, clangd needs `compile_commands.json`

**Generate `compile_commands.json`**

```bash
bear -- make
```

**"No information available" on hover (`K`)**
- LSP still indexing
- Standalone file without project structure

### Restart LSP

```vim
:LspRestart
:LspStop
:LspStart
```
