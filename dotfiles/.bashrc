
# If not running interactively, don't do anything
[[ $- != *i* ]] && return

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
for p in /usr/share/git/completion/git-prompt.sh /etc/bash_completion.d/git-prompt.sh /mingw64/share/git/completion/git-prompt.sh; do
  [ -f "$p" ] && source "$p" && break
done
export GIT_PS1_SHOWDIRTYSTATE=1
export PS1="\[\e[32m\]\u@\h \[\e[36m\]\W\[\e[31m\]\$(__git_ps1 ' (%s)')\[\e[32m\]\$ "

# iw
export PATH=$PATH:/usr/sbin

