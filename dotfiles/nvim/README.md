# slim-vim

A slim neovim configuration based on [kickstart.nvim](https://github.com/nvim-lua/kickstart.nvim)

## Features

- **Telescope-Powered Navigation**: Fast fuzzy finding for files, symbols, and help
- **Syntax Highlighting**: Modern syntax highlighting via Treesitter
- **Streamlined LSP Integration**: Core language server support for C and Haskell
- **Which-Key Integration**: Popup help for keybindings as you type them
- **Gitsigns in Gutter**: Instantly see added, changed, or deleted lines with minimal, colored indicators

## Behavior

**Autosave**: Files are automatically saved when you leave insert mode or make changes in normal mode

## Key Plugins

| Plugin | Purpose | Why It's Included |
|--------|---------|-------------------|
| **telescope.nvim** | Fuzzy finding | Essential navigation for files, symbols, and help |
| **nvim-treesitter** | Syntax highlighting | Better code understanding with minimal overhead |
| **nvim-lspconfig** | Language server setup | Code intelligence via system-installed LSP servers |
| **mini.nvim** | Statusline and text tools | Lightweight alternative to multiple separate plugins |
| **which-key.nvim** | Keybinding help | Discoverability without memorization |
| **gitsigns.nvim** | Git change indicators in gutter | Minimal, fast, and visually clear git status; includes interactive diff mode |


## Keymaps

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

### Git

| Mapping | Description |
|---------|-------------|
| `]c` / `[c` | Next/previous hunk (works in diff mode or normal editing) |
| `<leader>gd` | Preview hunk diff inline |
| `<leader>gr` | Reset hunk (undo changes in current hunk) |
| `<leader>gR` | Reset buffer (undo all changes in file) |
| `<leader>sc` | Search commits (view git history - Telescope) |

**Gitsigns** provides visual indicators in the gutter:
- `+` Added lines (opulent olive)
- `~` Changed lines (ambiguous amber)
- `_` Deleted lines (bold bordeaux)

**Git workflow**:
1. See changes: Gutter signs show what's modified
2. Review hunk: `<leader>gd` shows inline diff
3. Navigate: `]c` / `[c` to jump between hunks
4. Undo hunk: `<leader>gr` to reset current hunk
5. Undo all: `<leader>gR` to reset entire file

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

### Completions

This config uses manual completions for a minimal experience. Press `<C-x>` followed by another key to trigger different completion modes:

| Mapping | Description | Example |
|---------|-------------|---------|
| `<C-x><C-o>` | LSP/Omni completion | Functions, variables from language server |
| `<C-x><C-n>` | Current buffer keywords | Words in the current file |
| `<C-x><C-l>` | Line completion | Entire lines from file |

**Navigation**: `<C-n>` next, `<C-p>` previous, `<C-y>` accept, `<C-e>` cancel.

## Appendix: Vim Basics

### Editing

| Mapping | Description |
|---------|-------------|
| `.` | Repeat last change |
| `u` | Undo previous |
| `U` | Undo all changes on line |
| `<C-r>` | Redo undone |
| `c [number] motion` | Change text specified by [number] and motion |
| `R` | Enters Replace mode until `<Esc>` is pressed |
| `D` | Delete remainder of line |
| `I` | Insert at first non-blank character of line |
| `gI` | Insert at column 1 of line |

### Text Manipulation

| Mapping | Description |
|---------|-------------|
| `guu` | Lowercase entire line |
| `gUU` | Uppercase entire line |
| `guw` | Lowercase word |
| `gUw` | Uppercase word |
| `gJ` | Join lines without adding space |
| `gq` | Format/wrap text (motion) |

### Text Objects

Text objects allow operations (change, delete, yank, visual select) on structured text like words, sentences, or delimited blocks.

| Mapping | Description |
|---------|-------------|
| `ci"` | Change inside double quotes |
| `di(` | Delete inside parentheses |
| `vi[` | Visual select inside brackets |
| `caw` | Change around word (includes whitespace) |
| `vaw` | Visual select around word |
| `diw` | Delete inner word |
| `vip` | Visual select inner paragraph |
| `vap` | Visual select around paragraph |
| `cis` | Change inside sentence |
| `vas` | Visual select around sentence |

### Visual Mode

| Mapping | Description |
|---------|-------------|
| `<C-v>` | Visual block mode - select rectangular blocks of text |
| `gv` | Reselect last visual selection |
| `vey` | Yank from cursor to end of word |
| `vep` | Paste over from cursor to end of word |
| `!nl` | Number selected lines (when in visual mode) |
| `!uniq` | Remove duplicate lines (from visual selection) |
| `v motion :w FILENAME` | Save the Visually selected lines in file FILENAME |
| `v motion :w >> FILENAME` | Append the Visually selected lines to file FILENAME |

### Search & Replace

| Mapping | Description |
|---------|-------------|
| `:%s/old/new/gc` | Global find and replace with confirmation |
| `:g/pattern/d` | Delete all lines matching pattern |
| `:v/pattern/d` | Delete all lines NOT matching pattern |
| `%!uniq` | Remove duplicate lines from entire file |
| `*` | Search forward for word under cursor |
| `#` | Search backward for word under cursor |

### Macros

Macros allow recording and replaying sequences of commands.

| Mapping | Description |
|---------|-------------|
| `q<register>` | Start recording macro to a register (e.g., `qa`) |
| `q` | Stop recording macro |
| `@<register>` | Replay macro from register (e.g., `@a`) |
| `@@` | Replay the last executed macro |

### Registers

Registers store text for yanking, deleting, or pasting (like multiple clipboards).

| Mapping | Description |
|---------|-------------|
| `"a` | Use register 'a' for yank/delete/paste (replace 'a' with any letter) |
| `:reg` | View contents of all registers |

### File Operations & Buffers

| Mapping | Description |
|---------|-------------|
| `:b[uffer] [name/number]` | Switch to a buffer |
| `:bn[ext]` | Switch to the next buffer |
| `:bp[revious]` | Switch to the previous buffer |
| `:bd[elete]` | Unload buffer and remove it from list |
| `:r FILENAME` | Retrieves disk file and puts it below the cursor |
| `:r !command` | Insert output of shell command below cursor (ex: date) |
| `:x,yw >> FILENAME` | Appends lines x-y to file FILENAME |
| `gf` | Go to file under cursor |
| `gx` | Open URL under cursor |

### Navigation

**Jump List** - Tracks significant cursor movements (searches, go to definition, file jumps):

| Mapping | Description |
|---------|-------------|
| `<C-o>` | Jump to older position (back) |
| `<C-i>` or `<Tab>` | Jump to newer position (forward) |
| `<C-6>` | Toggle between last two files (use `6` on non-US keyboards) |
| `:jumps` | View full jump list |

**Change List**:

| Mapping | Description |
|---------|-------------|
| `g;` | Jump to last change position |
| `g,` | Jump to newer change position |

**Scrolling**:

| Mapping | Description |
|---------|-------------|
| `<C-d>` | Scroll down half a page |
| `<C-u>` | Scroll up half a page |
| `zt` | Scroll screen to put cursor at top |
| `zz` | Scroll screen to put cursor at middle |
| `zb` | Scroll screen to put cursor at bottom |

**Screen Movement**:

| Mapping | Description |
|---------|-------------|
| `H` | Move cursor to Highest line on screen |
| `M` | Move cursor to Middle line on screen |
| `L` | Move cursor to Lowest line on screen |

**Line & Word Navigation**:

| Mapping | Description |
|---------|-------------|
| `f{char}` / `F{char}` | Find character forward/backward in current line |
| `;` | Repeat last `f` or `F` in the same direction |
| `,` | Repeat last `f` or `F` in the opposite direction |

**Other**:

| Mapping | Description |
|---------|-------------|
| `gi` | Insert at last insert position |

**Split Windows**:

| Mapping    | Action                        |
|------------|-------------------------------|
| `<C-h>`    | Move focus to left split      |
| `<C-l>`    | Move focus to right split     |
| `<C-j>`    | Move focus to lower split     |
| `<C-k>`    | Move focus to upper split     |
| `<A-h>`    | Resize split left             |
| `<A-l>`    | Resize split right            |
| `<A-j>`    | Resize split down             |
| `<A-k>`    | Resize split up               |

### Diff Mode

**Commands**:

```vim
:diffsplit %              " Compare with saved file
:w !diff % -              " Show changes
:w !diff -u % -           " Show changes with context
:w !colordiff % -         " Colorized diff (if colordiff installed)
```

**Navigation**:

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
