# Bash Configuration

This directory contains configuration files for the Bash shell.

## Features

- **Vi Mode**: Uses Bash's vi-style keybindings for command line editing
- **History Management**: Ignores duplicates and erased commands
- **Custom Aliases**: Shortcuts for common commands and Git operations
- **Productivity Functions**: Helper functions for directory navigation and file operations

## Key Bindings

### Vi mode basics

| Keybinding | Action |
|------------|--------|
| `Esc`      | Switch to command mode |
| `i`        | Enter insert mode |
| `/`        | Search command history |
| `n`        | Next search match |
| `N`        | Previous search match |
| `k`        | Previous command in history |
| `j`        | Next command in history |

## Aliases

### Commands
- `ll`:     Long directory listing with timestamps
- `v`:      Open Neovim
- `ret`:    Print last exit code
- `ee`:     echo
- `ss`:     sudo rerun

### Git
- `gs`:     status
- `ga`:     add
- `gaa`:    add all
- `gcm`:    commit with message
- `gp`:     push
- `gl`:     log (oneline, graph, decorate)
- `gco`:    checkout
- `gcb`:    checkout new branch
- `gd`:     diff
- `gds`:    diff staged
- `gpo`:    pull origin
- `gr`:     restore
- `gcl`:    clone
- `gsta`:   stash
- `gstp`:   stash pop

## Functions

- `cdc <directory>`: Change to directory and open in VS Code
- `cdv <directory>`: Change to directory and open in Neovim
- `mkcd <directory>`: Create directory and change into it
- `bak <file>`: Create a backup copy of file (appends .bak)
- `ff <pattern> [directory]`: Find files matching pattern (default current directory)

## Environment Variables

- `HISTCONTROL`: Ignore duplicates and commands starting with space
- `GIT_PS1_SHOWDIRTYSTATE`: Show Git repository dirty state in prompt
- `PATH`: Extended with custom binary paths (iw, Haskell, Rust)</content>
<parameter name="filePath">/home/holmen1/repos/dotfiles/dotfiles/bash/README.md
