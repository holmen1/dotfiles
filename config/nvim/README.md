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

## Appendix: Vim basics

### Less known, very useful

1. `<C-o>` in INSERT: execute a single NORMAL mode command.
2. `.`: repeat your last change.
3. `vip`: visually select the inner paragraph, then `y` or `c`.
4. `<C-v>`: visual block mode.
5. `<C-a>` / `<C-x>`: increment or decrement numbers.
6. `gv`: reselect your previous visual selection.
7. `<C-o>` / `<C-i>`: jump back and forward in the jump list.
8. `!!sh` : normal-mode shorthand for `:.!sh`, replace line with shell output

### Editing

| Mapping | Description |
|---------|-------------|
| `.` | Repeat last change |
| `u` | Undo previous change |
| `<C-r>` | Redo undone change |
| `U` | Undo all changes on the current line |
| `R` | Replace mode until `<Esc>` |
| `D` | Delete to end of line |
| `I` | Insert at first non-blank character |
| `gI` | Insert at column 1 |
| `<C-t>` | Shift indent right in INSERT mode |
| `<C-d>` | Shift indent left in INSERT mode |
| `<C-f>` | Re-evaluate auto-indentation in INSERT mode |
| `gJ` | Join lines without adding space |
| `gq` | Format or wrap text |

### Text manipulation

| Mapping | Description |
|---------|-------------|
| `gc[c]` | Comment selection or line |
| `~` | Toggle case |
| `~{motion}` | Toggle case for motion |
| `guu` | Lowercase current line |
| `gUU` | Uppercase current line |
| `guw` | Lowercase current word |
| `gUw` | Uppercase current word |
| `ci"` | Change inside double quotes |
| `di(` | Delete inside parentheses |
| `vi[` | Visual select inside brackets |
| `caw` | Change around word |
| `vaw` | Visual select around word |
| `diw` | Delete inner word |
| `vip` | Visual select inner paragraph |
| `vap` | Visual select around paragraph |
| `cis` | Change inside sentence |
| `vas` | Visual select around sentence |

### Visual mode

| Mapping | Description |
|---------|-------------|
| `<C-v>` | Visual block mode |
| `o` / `O` | Swap cursor to the other end of the selection |
| `gv` | Reselect last visual selection |
| `vey` | Yank from cursor to end of word |
| `vep` | Paste over from cursor to end of word |
| `!nl` | Number selected lines |
| `!uniq` | Remove duplicate lines from selection |
| `v motion :w FILENAME` | Save selected lines to a file |
| `v motion :w >> FILENAME` | Append selected lines to a file |

### Insert mode

| Mapping | Description |
|---------|-------------|
| `<C-o>` | Execute one NORMAL mode command |
| `<C-w>` | Delete previous word |
| `<C-h>` | Delete previous character |
| `<C-u>` | Delete back to the start of line |

### Search and replace

| Mapping | Description |
|---------|-------------|
| `:%s/old/new/gc` | Global find and replace with confirmation |
| `:g/pattern/d` | Delete all lines matching pattern |
| `:v/pattern/d` | Delete all lines not matching pattern |
| `%!uniq` | Remove duplicate lines from entire file |
| `*` | Search forward for word under cursor |
| `#` | Search backward for word under cursor |

### Macros

| Mapping | Description |
|---------|-------------|
| `q<register>` | Start recording a macro |
| `q` | Stop recording macro |
| `@<register>` | Replay macro from register |
| `@@` | Replay the last executed macro |

### Registers

| Mapping | Description |
|---------|-------------|
| `"a` | Use register `a` for yank/delete/paste |
| `:reg` | View register contents |

### File operations and buffers

| Mapping | Description |
|---------|-------------|
| `:b[uffer] [name/number]` | Switch buffers |
| `C-l' or ':bn[ext]` | Next buffer |
| `C-h' or ':bp[revious]` | Previous buffer |
| `:bd[elete]` | Unload current buffer |
| `:r FILENAME` | Read file into buffer |
| `:r !command` | Insert output of command |
| `gf` | Go to file under cursor |
| `gx` | Open URL under cursor |
| `:.!sh` | Replace line with shell output |
| `!!sh` | Normal-mode shorthand for `:.!sh` |
| `:.w !sh` | Pipe current line to shell |
| `:'<,'>w !sh` | Pipe visual selection to shell |

### Navigation

**Jump list**

| Mapping | Description |
|---------|-------------|
| `<C-o>` | Jump to older position |
| `<C-i>` / `<Tab>` | Jump to newer position |
| `<C-6>` | Toggle between last two files |
| `:jumps` | View jump list |

**Change list**

| Mapping | Description |
|---------|-------------|
| `g;` | Jump to last change position |
| `g,` | Jump to newer change position |

**Scrolling**

| Mapping | Description |
|---------|-------------|
| `<C-d>` | Scroll down half a page |
| `<C-u>` | Scroll up half a page |
| `zt` | Put cursor line at top |
| `zz` | Put cursor line in middle |
| `zb` | Put cursor line at bottom |

**Screen movement**

| Mapping | Description |
|---------|-------------|
| `H` | Move to highest line on screen |
| `M` | Move to middle line on screen |
| `L` | Move to lowest line on screen |

**Line and word navigation**

| Mapping | Description |
|---------|-------------|
| `f{char}` / `F{char}` | Find character forward/backward |
| `;` | Repeat last `f` or `F` |
| `,` | Repeat last `f` or `F` in reverse |
| `gi` | Return to last insert position |

### Split windows

| Mapping | Action |
|---------|--------|
| `<C-h>` | Move focus to left split |
| `<C-l>` | Move focus to right split |
| `<C-j>` | Move focus to lower split |
| `<C-k>` | Move focus to upper split |
| `<A-h>` | Resize split left |
| `<A-l>` | Resize split right |
| `<A-j>` | Resize split down |
| `<A-k>` | Resize split up |

### Diff mode

**Commands**

```vim
:diffsplit %              " Compare with saved file
:w !diff % -              " Show changes
:w !diff -u % -           " Show changes with context
:w !colordiff % -         " Colorized diff if installed
```

**Navigation**

| Mapping | Action |
|---------|--------|
| `]c` | Jump to next difference |
| `[c` | Jump to previous difference |
| `do` | Obtain changes from other window |
| `dp` | Put changes to other window |
| `zr` | Expand folds to see more context |

### Completions

This config keeps completion simple. Press `<C-x>` plus another key:

| Mapping | Description | Example |
|---------|-------------|---------|
| `<C-x><C-o>` | LSP / omni completion | Functions and symbols from the language server |
| `<C-x><C-n>` | Current-buffer keywords | Words in the current file |
| `<C-x><C-l>` | Line completion | Entire lines from the file |

**Navigation:** `<C-n>` next, `<C-p>` previous, `<C-y>` accept, `<C-e>` cancel.

### Checkhealth

If the config behaves strangely, use:

```bash
nvim --headless -c "checkhealth" -c "qa"
```

### LSP troubleshooting

This config bypasses Mason entirely. LSP servers are installed via the system package manager.

#### Install LSP servers

| Server | Artix | FreeBSD |
|--------|------|---------|
| clangd | `clang` | `llvm` |
| lua_ls | `lua-language-server` | `lua-language-server` |
| hls | `extra/haskell-language-server` | `hs-haskell-language-server` |
| bash | `extra/bash-language-server`, `extra/shfmt` | `hs-ShellCheck`, `shfmt` (not tested) |

#### Check status

```vim
:LspLog
:checkhealth vim.lsp
```

#### Common issues

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

#### Restart LSP

```vim
:LspRestart
:LspStop
:LspStart
```
