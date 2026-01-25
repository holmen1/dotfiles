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
| **nvim-lspconfig** | Language server setup | Code intelligence via system-installed LSP servers |
| **mini.nvim** | Statusline and text tools | Lightweight alternative to multiple separate plugins |
| **which-key.nvim** | Keybinding help | Discoverability without memorization |
| **gitsigns.nvim** | Git change indicators in gutter | Minimal, fast, and visually clear git status |


## Keymaps

### vim

Reminder
| Mapping | Description |
|---------|-------------|
| `u` | Undo previous |
| `U` | Undo all changes on line |
| `<C-r>` | Redo undone |
| `c [number] motion` | Change text specified by [number] and motion |
| `R` | Enters Replace mode until `<Esc>`{normal} is pressed |
| `v motion :w FILENAME` | Saves the Visually selected lines in file FILENAME |
| `v motion :w >> FILENAME` | Appends the Visually selected lines to file FILENAME |
| `<C-v>` | Visual block mode - select rectangular blocks of text |
| `gv` | Reselect last visual selection |
| `!nl` | Number selected lines (when in visual mode) |
| `:x,yw >> FILENAME` | Appends lines x-y to file FILENAME |
| `:r FILENAME` | Retrieves disk file FILENAME and puts it below the cursor |
| `:r !command`|	Insert output of shell command below cursor (ex date)|
| `!uniq` | Remove duplicate lines (from visual selection) |
| `%!uniq` | Remove duplicate lines from entire file |
| `:%s/old/new/gc`| Global find and replace with confirmation |
| `:g/pattern/d` | Delete all lines matching pattern |
| `:v/pattern/d` | Delete all lines NOT matching pattern |



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

### Diagnostics
| Mapping | Description |
|---------|-------------|
| `<leader>q` | Open diagnostic quickfix list |





### LSP Navigation & Actions
| Mapping | Description | When to Use |
|---------|-------------|------------|
| `K` | Hover documentation | Show docs for symbol under cursor |
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


### Navigate Split Window

| Mapping    | Action                        |
|------------|-------------------------------|
| Ctrl+h     | Move focus to left split      |
| Ctrl+l     | Move focus to right split     |
| Ctrl+j     | Move focus to lower split     |
| Ctrl+k     | Move focus to upper split     |
| Alt+h      | Resize split left             |
| Alt+l      | Resize split right            |
| Alt+j      | Resize split down             |
| Alt+k      | Resize split up               |



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

This config bypasses Mason entirely - LSP servers are installed via system package manager.

### Install LSP Servers

| Server | Arch | FreeBSD |
|--------|------|---------|
| clangd | `pacman -S clang` | `pkg install llvm` |
| lua_ls | `pacman -S lua-language-server` | Build from source |
| hls | `ghcup install hls` | `pkg install hs-haskell-language-server` |

### Check Status

```vim
:LspInfo              " View attached servers
:LspLog               " Check for errors
:checkhealth lsp      " Full diagnostics
```

### Common Issues

**"No LSP implementation found"**
- Server not installed: `which clangd haskell-language-server-wrapper`
- Server not in PATH: Check shell profile
- Not a project: HLS needs `*.cabal`, clangd needs `compile_commands.json`

**Generate compile_commands.json for C/C++:**
```bash
bear -- make
```

**"No information available" on hover (K)**
- LSP still indexing - wait for it to finish
- Standalone file without project structure

### Force Restart

```vim
:LspRestart
:LspStop
:LspStart
```
