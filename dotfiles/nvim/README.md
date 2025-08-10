# slim-vim

A slim neovim configuration based on [kickstart.nvim](https://github.com/nvim-lua/kickstart.nvim)

## Features

- **Telescope-Powered Navigation**: Fast fuzzy finding for files, symbols, and help
- **Syntax Highlighting**: Modern syntax highlighting via Treesitter
- **Streamlined LSP Integration**: Core language server support for C and Haskell
- **Which-Key Integration**: Popup help for keybindings as you type them
- **Gitsigns in Gutter**: Instantly see added, changed, or deleted lines with minimal, colored indicators

## Key Plugins

| Plugin | Purpose | Why It's Included |
|--------|---------|-------------------|
| **telescope.nvim** | Fuzzy finding | Essential navigation for files, symbols, and help |
| **nvim-treesitter** | Syntax highlighting | Better code understanding with minimal overhead |
| **lspconfig & mason.nvim** | Language server setup | Code intelligence |
| **mini.nvim** | Statusline and text tools | Lightweight alternative to multiple separate plugins |
| **which-key.nvim** | Keybinding help | Discoverability without memorization |
| **gitsigns.nvim** | Git change indicators in gutter | Minimal, fast, and visually clear git status |


## Keymaps


### Diagnostics
| Mapping | Description |
|---------|-------------|
| `<leader>q` | Open diagnostic quickfix list |

### Telescope
| Mapping | Description | Function |
|---------|-------------|----------|
| `<leader>sf` | Search Files | Find files in current directory |
| `<leader>sg` | Search by Grep | Search file contents (live grep) |
| `<leader>sw` | Search current Word | Find occurrences of word under cursor |
| `<leader>sr` | Search Resume | Resume previous search |
| `<leader>ss` | Search Select Telescope | Show all available Telescope commands |
| `<leader>sd` | Search Diagnostics | Show all diagnostics |
| `<leader>s.` | Search Recent Files | Show recently opened files |
| `<leader><leader>` | Find Buffers | Switch between open files/buffers |
| `<leader>sc` | Search Commits | View git history for current file |

| Mapping | Description | Details |
|---------|-------------|---------|
| `<leader>/` | Search in Current Buffer | Fuzzy find in current file with dropdown UI |
| `<leader>s/` | Search in Open Files | Live grep limited to open files only |
| `<leader>sn` | Search Neovim Config | Find files in your Neovim config directory |



### LSP Navigation & Actions
| Mapping | Description | When to Use |
|---------|-------------|------------|
| `grn` | Rename symbol | Rename variables/functions across files |
| `gra` | Code action | Fix errors, organize imports |
| `grr` | Find references | See all usages of a symbol |
| `gri` | Go to implementation | Jump to implementation (vs declaration) |
| `grd` | Go to definition | Jump to where a symbol is defined |
| `grD` | Go to declaration | Jump to declaration (useful in C/C++ for headers) |
| `grt` | Go to type definition | See a variable's type definition |
| `gO` | Document symbols | Browse all symbols in current file |
| `gW` | Workspace symbols | Browse all symbols in project |
| `<leader>f` | Format code | Format current buffer or selection |
| `<leader>th` | Toggle inlay hints | Show/hide inline type hints |



####  Compare current buffer with saved file

```vim
:diffsplit %
```

```vim
:w !diff % -
``
Show changes with context
```vim
:w !diff -u % -
```

Colorized diff (if colordiff is installed)
```vim
:w !colordiff % -
```
Keyboard Navigation in Diff Mode

| Mapping | Action |
|---------|--------|
| `]c` | Jump to next difference |
| `[c` | Jump to previous difference |
| `do` | Obtain changes (from other window) |
| `dp` | Put changes (to other window) |
| `zr` | Expand folds to see more context |


## LSP Troubleshooting

Having issues with language servers? Here's a step-by-step guide to diagnose and fix common problems:

### Check Installation and Status

```vim
:Mason                " Check if language servers are installed
:LspInfo              " View active language servers for current buffer
:LspLog               " Check for errors in LSP log
```

### Common Issues and Fixes

#### Language Server Not Installing


If Mason fails to install a language server:

```bash
# For Haskell Language Server, try direct installation
ghcup install hls    # Install via GHCup

# For C/C++, ensure clangd is available
sudo apt install clangd    # On Debian/Ubuntu
```
### C/C++: No LSP implementation found

For full language server (LSP) support in C/C++, you need a `compile_commands.json` file in your project root. If your build system doesn't generate this automatically, use [`bear`](https://github.com/rizsotto/Bear):

```sh
bear -- make
```


#### Language Server Not Attaching

**Force server start**
```vim
:LspStart hls      " For Haskell
:LspStart clangd   " For C/C++
```

#### Advanced LSP Commands

```vim
:LspRestart           " Restart LSP servers
:LspStop              " Stop LSP servers
:LspInstall <server>  " Install a specific server
```


### Check Log for Errors

Look for errors in the LSP log:

```
:LspLog
```

Then add this to your LSP configuration:

```lua
hls = {
  cmd = { "haskell-language-server-wrapper", "--lsp" }
}
```
