# slim-vim

A slim neovim configuration based on [kickstart.nvim](https://github.com/nvim-lua/kickstart.nvim)

## Features

- **Telescope-Powered Navigation**: Fast fuzzy finding for files, symbols, and help
- **Syntax Highlighting**: Modern syntax highlighting via Treesitter
- **Streamlined LSP Integration**: Core language server support for C and Haskell
- **Which-Key Integration**: Popup help for keybindings as you type them
- **Terminal Integration**: Built-in terminal with easy mode switching

## Key Plugins

| Plugin | Purpose | Why It's Included |
|--------|---------|-------------------|
| **telescope.nvim** | Fuzzy finding | Essential navigation for files, symbols, and help |
| **nvim-treesitter** | Syntax highlighting | Better code understanding with minimal overhead |
| **lspconfig & mason.nvim** | Language server setup | Code intelligence |
| **mini.nvim** | Statusline and text tools | Lightweight alternative to multiple separate plugins |
| **which-key.nvim** | Keybinding help | Discoverability without memorization |


## Keymaps

### Window Navigation
| Mapping | Description |
|---------|-------------|
| `<C-h>` | Move focus to the left window |
| `<C-j>` | Move focus to the lower window |
| `<C-k>` | Move focus to the upper window |
| `<C-l>` | Move focus to the right window |

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


## Terminal Integration

Neovim's built-in terminal emulator lets you run shell commands without leaving the editor

| Command | Action |
|---------|--------|
| `:terminal` | Open terminal in current window |
| `:split \| terminal` | Open terminal in horizontal split |
| `:vsplit \| terminal` | Open terminal in vertical split |
| `:terminal [cmd]` | Run specific command (e.g., `:terminal python`) |

### Terminal Mode Navigation

| Mapping | Action |
|---------|--------|
| `<C-\\><C-n>` | Exit terminal mode (custom mapping) |
| `i` or `a` | Enter terminal mode from normal mode |
| `<C-h/j/k/l>` | Navigate between windows (in normal mode) |
| `<C-u>` / `<C-d>`| Scroll up/down half-page |
| `<C-b>` / `<C-f>`| Scroll up/down full-page |
| `{` / `}`| Jump between paragraphs |
| `:bd!` | Close a terminal buffer completely |


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

This structured approach should help resolve most LSP issues while keeping your slim-vim configuration clean and functional.-----






I see you've configured HLS in your LSP setup, but it might not be loading properly. Here's how to test and fix it:

## 1. Check if HLS is Installed

First, verify HLS is properly installed via Mason:

```
:Mason
```

Look for `haskell-language-server` in the list:
- If it's marked with ✓, it's installed
- If not, press `i` on the entry to install it

## 2. Check if HLS is Attaching to Your File

Open your Haskell file and check which LSP servers are attached:

```
:LspInfo
```

This will show all active language servers for the current buffer. If HLS is missing, that's the issue.

## 3. Test Common Issues

### Filetype Recognition

Make sure Neovim recognizes your file as Haskell:

```
:set filetype?
```

It should show `filetype=haskell`. If not, manually set it:

```
:set filetype=haskell
```

### File Extension

Ensure your file has `.hs` extension. HLS typically only attaches to files with proper extensions.

### Force LSP Attachment

Try manually starting LSP for the current buffer:

```
:LspStart hls
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
