

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
alias ll='ls -lat --color=auto'
alias grep='grep --color=auto'
alias ..='cd ..'
alias cdr='cd ~/repos'

export HISTCONTROL=ignoreboth:erasedups

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
alias gcl='git clone'
alias gsta='git stash'
alias gstp='git stash pop'

# Git branch and status in the prompt
parse_git_branch() {
    git branch 2>/dev/null | grep '^*' | colrm 1 2
}

parse_git_dirty() {
    [[ $(git status --porcelain 2>/dev/null) ]] && echo "*"
}

export PS1="\[\e[32m\]\u@\h \[\e[33m\]\w\[\e[36m\] \$(parse_git_branch)\$(parse_git_dirty) \[\e[0m\]\$ "

# iw
export PATH=$PATH:/usr/sbin

# Add custom bin directory to PATH (used by xmonad build)
export PATH=$HOME/.local/bin:$PATH
export PATH=$HOME/.ghcup/bin:$PATH

[ -f "~/.ghcup/env" ] && . "~/.ghcup/env" # ghcup-env
