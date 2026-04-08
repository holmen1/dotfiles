# Bash Configuration

This directory contains configuration files for the Bash shell.

## Features

- **Colored Prompt**: Displays user, host, current directory, and Git status with custom colors
- **Emacs Mode**: Uses Bash's default Emacs-style keybindings for efficient command line editing
- **Case-Insensitive Completion**: Tab completion ignores case
- **History Management**: Ignores duplicates and erased commands
- **Custom Aliases**: Shortcuts for common commands and Git operations
- **Productivity Functions**: Helper functions for directory navigation and file operations

## Installation

To install the Bash configuration, run the link script from the repository root:

```bash
scripts/link_config.sh
```

This will symlink the `.bashrc` file to your home directory.

## Key Bindings

| Keybinding | Action |
|------------|--------|
| `C-p` or `↑` | Previous command in history |
| `C-n` or `↓` | Next command in history |
| `C-r` | Reverse incremental search (type to search backward) |
| `C-a` | Move to beginning of line |
| `C-e` | Move to end of line |
| `C-k` | Kill (cut) from cursor to end of line |
| `C-u` | Kill from cursor to beginning of line |
| `C-y` | Yank (paste) last killed text |
| `Tab` | Autocomplete filename/command |
| `C-d` | Delete character forward (or exit if line is empty) |

## Aliases

### Navigation
- `..`: Go up one directory
- `cdr`: Change to `~/repos` directory

### Commands
- `ls`: Colored directory listing
- `ll`: Long directory listing with timestamps
- `grep`: Colored grep output
- `v`: Open Neovim
- `ff`: Run fastfetch
- `diff`: Colored diff output
- `less`: Less with raw control characters
- `ret`: Print last exit code

### System
- `reboot`: System reboot (requires sudo)
- `shutdown`: System shutdown (requires sudo)

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

- `LS_COLORS`: Custom colors for `ls` output
- `GREP_COLORS`: Custom colors for `grep` output
- `HISTCONTROL`: Ignore duplicates and commands starting with space
- `GIT_PS1_SHOWDIRTYSTATE`: Show Git repository dirty state in prompt
- `PATH`: Extended with custom binary paths (iw, Haskell, Rust)</content>
<parameter name="filePath">/home/holmen1/repos/dotfiles/dotfiles/bash/README.md