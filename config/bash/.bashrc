## Features
# **Vi Mode**: Uses Bash's vi-style keybindings for command line editing
# **History Management**: Ignores duplicates and erased commands
# **Custom Aliases**: Shortcuts for common commands and Git operations
# **Productivity Functions**: Helper functions for directory navigation and file operations

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Color definitions
export COLOR_RESET='\[\e[0m\]'           # Proper ANSI reset
export COLOR_USER='\[\e[38;5;24m\]'      # Steel blue
export COLOR_PATH='\[\e[38;5;33m\]'      # Electric blue
export COLOR_GIT='\[\e[38;5;60m\]'       # Dark slate
export COLOR_PROMPT='\[\e[38;5;40m\]'    # Forest green

export LS_COLORS='di=38;5;32:fi=38;5;244:ln=38;5;60:ex=38;5;40:*.sh=38;5;40:*.py=38;5;24:*.js=38;5;60'
export GREP_COLORS='ms=38;5;33:fn=38;5;60:ln=38;5;24'

# Auto  cd
shopt -s autocd

# Enable vi mode in bash
set -o vi
bind -m vi-insert 'Control-l: clear-screen'
# Key | Action                  |
#-----|-------------------------|
# Esc | Switch to command mode  |
# i   | Enter insert mode       |
# /   | Search command history  |
# n   | Next search match       |
# N   | Previous search match   |
# k   | Previous command in history |
# j   | Next command in history |

# Make Tab autocomplete regardless of filename case
bind 'set completion-ignore-case on'

# Arrow key history search
bind '"\e[A": history-search-backward'
bind '"\e[B": history-search-forward'

export HISTCONTROL=ignoreboth:erasedups # Ignore duplicates and commands starting with space

### Custom functions
# Open current directory in VSCode or Neovim
cdc() {
	cd "$1" && code .
}
cdv() {
	cd "$1" && nvim .
}
# Create and change into a new directory
mkcd() {
    mkdir -p "$1" && cd "$1"
}
# Create backup file
bak() {
    cp "$1" "$1.bak"
}
# Quick file search function (rename from ff to avoid conflict)
ff() {
    find "${2:-.}" -name "*$1*" 2>/dev/null
}
# Echo variable
ee() {
    echo "${!1}"
}
# Repeat last command with sudo
ss() { sudo "$(history -p !!)" ; }

### Aliases
alias ls='ls --color=auto'
alias ll='ls -lath --color=auto'
alias gg='grep --color=auto'
alias ..='cd ..'
alias reboot='sudo reboot'
alias shutdown='sudo shutdown'
alias v='nvim'
alias c='code .'
alias diff='diff --color=auto'
alias less='less -R'
alias ret='echo $?'
alias sudo='sudo ' # Allow alias expansion after sudo
alias pp='ping -c 4'
alias tt='tree -aL 2'

# Git Aliases
alias gs='git status'
alias ga='git add'
alias gaa='git add --all'
alias gcm='git commit -m'
alias gp='git push'
alias gl='git log --oneline --graph --decorate'
alias gco='git checkout'
alias gcb='git checkout -b'
alias gd='git diff'
alias gds='git diff --staged'
alias gpo='git pull origin'
alias gpr='git pull --rebase' # When behind remote
alias gr='git restore'
alias gcl='git clone'
alias gsta='git stash -u'
alias gstp='git stash pop'
gat() { git tag -a "$1" -m "$2" ; } # Annotated tag
# Then: git push origin vX.X

### Prompt
# source git-prompt if available (tries common locations)
for p in $HOME/.scripts/git-prompt.sh /usr/share/git/completion/git-prompt.sh /etc/bash_completion.d/git-prompt.sh /mingw64/share/git/completion/git-prompt.sh; do
  [ -f "$p" ] && source "$p" && break
done
export GIT_PS1_SHOWDIRTYSTATE=1 # Show Git repository dirty state in prompt
export PS1="${COLOR_USER}\u@\h ${COLOR_PATH}\W${COLOR_GIT}\$(__git_ps1 ' (%s)')${COLOR_PROMPT}\$ ${COLOR_RESET}"

### Paths
# iw
export PATH=$PATH:/usr/sbin
# Custom bin usd by haskell-language-server
export PATH="$HOME/.local/bin:$PATH"
# Cargo (Rust) binary path
export PATH="$HOME/.cargo/bin:$PATH"
# XMonad (cabal) binary path
export PATH="$HOME/.cabal/bin:$PATH"
