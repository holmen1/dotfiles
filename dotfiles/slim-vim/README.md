# Slim-Vim

A minimalist Neovim configuration based on kickstart.nvim, focused on essential features with reduced complexity.

## Features

- **Lightweight**: Only the essential plugins and configurations
- **Fast Startup**: Optimized for quick loading times
- **Core LSP Support**: Basic language server integration
- **Minimal UI**: Clean, distraction-free editing experience
- **Sensible Defaults**: Ready to use without extensive configuration

## Installation

```bash
# Backup existing configuration (if needed)
mv ~/.config/nvim ~/.config/nvim.bak

# Clone the repository
git clone https://github.com/username/slim-vim.git ~/.config/nvim
```

## Key Plugins

- **telescope.nvim**: Fuzzy finder
- **treesitter**: Syntax highlighting
- **lsp-zero**: Simplified LSP setup
- **nvim-cmp**: Minimal completion

## Keymaps

### Core Navigation
| Mapping | Description |
|---------|-------------|
| `<Space>` | Leader key |
| `<leader>sf` | Find files |
| `<leader>sg` | Live grep |
| `K` | Show hover documentation |
| `gd` | Go to definition |
| `<C-Space>` | Trigger completion |
| `<Esc>` | Clear search highlighting |

### Window Management
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


## Terminal


## Customization

Edit `~/.config/nvim/init.lua` to customize your configuration:

```lua
-- Add your personal settings here
vim.opt.relativenumber = true  -- Example setting
```

## Why Slim?

- **Focus**: Emphasizes what you need, not what you might want
- **Performance**: Faster startup and lower resource usage
- **Simplicity**: Easier to understand and maintain
- **Learning**: Better starting point for Neovim beginners

---

Fork of [kickstart.nvim](https://github.com/nvim-lua/kickstart.nvim) with unnecessary features removed.


# Your Slim Neovim Setup: Status Check

You've made great progress slimming down your configuration! Let's review what you've accomplished:


# Add Formatting Keymap to README

Here's the keymap for code formatting to add to your README.md's keymaps section:

```markdown
### Code Actions
| Mapping | Description |
|---------|-------------|
| `<leader>f` | Format current buffer |
| `<leader>ca` | Code action menu |
```

If these keymaps aren't already configured in your Neovim setup, you'll need to add them to your `keymaps.lua` file:

```lua
-- Format current buffer
vim.keymap.set('n', '<leader>f', function()
  vim.lsp.buf.format({ async = true })
end, { desc = 'Format current buffer' })

-- Code action
vim.keymap.set('n', '<leader>ca', function()
  vim.lsp.buf.code_action()
end, { desc = 'Code actions' })
```

or visual selection formatting, you can select text in Visual mode and use:
```
:'<,'>lua vim.lsp.buf.range_format()
```

This uses the formatting capabilities of your installed language servers without needing additional plugins.




# How to Use `:terminal` in Neovim

The built-in terminal emulator in Neovim is a powerful feature that lets you run shell commands without leaving the editor.

## Opening a Terminal

```vim
:terminal             " Open terminal in current window
:split | terminal     " Open terminal in horizontal split:
```

You can also specify a command to run:
```vim
:terminal ls -la      " Run ls -la in terminal
:terminal python      " Start Python REPL
```

## Terminal Mode vs Normal Mode

When a terminal opens, you're in **Terminal Mode** where:
- Keystrokes go to the terminal process
- You can type and interact as in a normal terminal

### Switching Modes

- **Terminal → Normal**: Press `<C-\\><C-n>` (lower left '\' on SV keymap)
- **Normal → Terminal**: Press `i` or `a` (like entering insert mode)

## Common Operations

### Window Navigation

While in Normal mode, use your configured window navigation keys:
```
<C-j>  " Move to bottom window
<C-k>  " Move to top window
```

### Copying Text

1. Enter Normal mode: `<Esc><Esc>`
2. Select text: `v` (visual mode)
3. Copy: `y`
4. Return to Terminal mode: `i` or `a`

### Scrolling History

In Normal mode:
- `<C-u>` / `<C-d>`: Scroll up/down half-page
- `<C-b>` / `<C-f>`: Scroll up/down full-page
- `{` / `}`: Jump between paragraphs

## Tips and Tricks

2. **Send commands from a buffer**:
   ```vim
   :source my_commands.vim | T  " T sends contents to terminal
   ```


## Real-World Examples

- **Running tests**: `:vsplit | terminal npm test`
- **File watching**: `:terminal watchexec -e js,jsx npm run build`
- **Database CLI**: `:terminal psql mydatabase`
- **SSH session**: `:terminal ssh user@server`


# How to Check Changes Before Saving in Neovim

There are several ways to review your changes before writing them to disk:

## 1. Using `diff` to Compare Changes

```vim
:w !diff % -
```

This command:
- `:w !` pipes the buffer content to a shell command
- `diff % -` compares the file on disk (`%`) with stdin (`-`)
- Shows differences between saved file and current buffer

## 2. Create a Buffer Diff

```vim
:diffsplit %
```

This will:
- Split the window
- Load the saved version of the file on disk
- Show differences between saved and current version

## 3. Check Modified Flag

Look at your status line - if there's a `[+]` symbol, the file has been modified.

