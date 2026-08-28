## Features
# **Vi Mode**: Uses Bash's vi-style keybindings for command line editing
# **History Management**: Ignores duplicates and erased commands
# **Custom Aliases**: Shortcuts for common commands and Git operations
# **Productivity Functions**: Helper functions for directory navigation and file operations

if command -v dash >/dev/null 2>&1; then
    sudo ln -sf /usr/bin/dash /usr/bin/sh
else
    echo "sh -> bash"
fi

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Color definitions (prompt uses \[...\] zero-width escapes for readline)
export COLOR_RESET='\[\e[0m\]'
export COLOR_USER='\[\e[38;5;243m\]'        # cool gray — user@host recedes
export COLOR_PATH='\[\e[38;5;180m\]'        # tan — readable, humble
export COLOR_GIT='\[\e[38;5;137m\]'         # warm ochre — branch
export COLOR_PROMPT_OK='\[\e[38;5;179m\]'   # gold amber — $ when last exit 0
export COLOR_PROMPT_ERR='\[\e[38;5;130m\]'  # deep red — $ when last exit != 0

export LS_COLORS='di=38;5;179:ln=38;5;185:ex=38;5;173:fi=38;5;187:or=31;01:*.sh=38;5;173:*.py=38;5;179:*.js=38;5;185'
export GREP_COLORS='ms=38;5;214:fn=38;5;180:ln=38;5;137'

## Default (plain/unstyled) text tint via OSC 10
# printf '\e]10;#d9b382\a'	# Amber-on-dark the standard on monochrome CRT terminals (DEC VT100/220, IBM 5151)
printf '\e]10;#e8cfa8\a'	# softer
# printf '\e]110;\a'		# To reset the default foreground back to st's config.h value:

# Auto cd
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
cdc() { # Open current directory in VSCode
	cd "$1" && code .
}
cdv() { # Open current directory in Neovim
	cd "$1" && nvim .
}
mkcd() { # Create and change into a new directory
    mkdir -p "$1" && cd "$1" || exit
}
bak() { # Create backup file
    cp -a "$1" "$1.bak"
}
ff() { # Quick file search function
    find "${2:-.}" -name "*$1*" 2>/dev/null
}
ee() { # Echo variable capitalized
    VAR=${1^^}
    echo "${!VAR}"
}
ss() { # Repeat last command with sudo
    sudo $(history -p !!)
}

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
alias s='sudo ' # Allow alias expansion after sudo
alias pp='ping -c 4'
alias tt='tree -aL 2'

# Git Aliases
alias gs='git status --short'
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
