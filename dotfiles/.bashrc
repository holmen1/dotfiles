
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


# Enable vi mode in bash
set -o vi

# Make Tab autocomplete regardless of filename case
bind 'set completion-ignore-case on'

# Make history searching work like vim
bind '"\e[A": history-search-backward'
bind '"\e[B": history-search-forward'

export HISTCONTROL=ignoreboth:erasedups

# Custom functions

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

# Quick file search function (rename from ff to avoid conflict)
fnd() {
    find "${2:-.}" -name "*$1*" 2>/dev/null
}


alias cdr='cd ~/repos'

alias ls='ls --color=auto'
alias ll='ls -lat --color=auto'
alias grep='grep --color=auto'
alias ..='cd ..'
alias reboot='sudo reboot'
alias shutdown='sudo shutdown'
alias v='nvim'
alias ff='fastfetch'
alias diff='diff --color=auto'
alias less='less -R'
alias ret='echo $?'

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
alias gr='git restore'
alias gcl='git clone'
alias gsta='git stash'
alias gstp='git stash pop'

# source git-prompt if available (tries common locations)
for p in $HOME/repos/dotfiles/scripts/git-prompt.sh /usr/share/git/completion/git-prompt.sh /etc/bash_completion.d/git-prompt.sh /mingw64/share/git/completion/git-prompt.sh; do
  [ -f "$p" ] && source "$p" && break
done
export GIT_PS1_SHOWDIRTYSTATE=1
export PS1="${COLOR_USER}\u@\h ${COLOR_PATH}\W${COLOR_GIT}\$(__git_ps1 ' (%s)')${COLOR_PROMPT}\$ ${COLOR_RESET}"

# iw
export PATH=$PATH:/usr/sbin

