# Bash Configuration

This directory contains configuration files for the Bash shell.

## Features

- **Emacs Mode**: Uses Bash's default Emacs-style keybindings for efficient command line editing
- **History Management**: Ignores duplicates and erased commands
- **Custom Aliases**: Shortcuts for common commands and Git operations
- **Productivity Functions**: Helper functions for directory navigation and file operations

## Key Bindings

| Keybinding | Action |
|------------|--------|
| `C-p` or `↑` | Previous command in history |
| `C-n` or `↓` | Next command in history |
| `C-r` | Reverse incremental search (type to search backward) |
| `C-l` | Clear |
| `C-a` | Move to beginning of line |
| `C-e` | Move to end of line |
| `C-k` | Kill (cut) from cursor to end of line |
| `C-u` | Kill from cursor to beginning of line |
| `C-y` | Yank (paste) last killed text |
| `C-d` | Delete character forward (or exit if line is empty) |

## Aliases

### Commands
- `ll`: Long directory listing with timestamps
- `v`: Open Neovim
- `ff`: Run fastfetch
- `diff`: Colored diff output
- `ret`: Print last exit code

### Git
- `gs`: Git status
- `ga`: Git add
- `gaa`: Git add all
- `gcm`: Git commit with message
- `gp`: Git push
- `gl`: Git log (oneline, graph, decorate)
- `gco`: Git checkout
- `gcb`: Git checkout new branch
- `gd`: Git diff
- `gds`: Git diff staged
- `gpo`: Git pull origin
- `gr`: Git restore
- `gcl`: Git clone
- `gsta`: Git stash
- `gstp`: Git stash pop

## Functions

- `cdc <directory>`: Change to directory and open in VS Code
- `cdv <directory>`: Change to directory and open in Neovim
- `mkcd <directory>`: Create directory and change into it
- `bak <file>`: Create a backup copy of file (appends .bak)
- `fnd <pattern> [directory]`: Find files matching pattern (default current directory)

## Environment Variables

- `HISTCONTROL`: Ignore duplicates and commands starting with space
- `GIT_PS1_SHOWDIRTYSTATE`: Show Git repository dirty state in prompt
- `PATH`: Extended with custom binary paths (iw, Haskell, Rust)</content>
<parameter name="filePath">/home/holmen1/repos/dotfiles/dotfiles/bash/README.md
