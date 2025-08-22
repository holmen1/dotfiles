# Minimal Zsh configuration

# NB: macOS-specific - uses scutil which is not available on Linux
# NB: macOS-specific - Linux uses --color=auto instead of -G
# NB: macOS-specific - Homebrew path for Apple Silicon Macs

# History configuration
HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000

# Basic Zsh options
setopt AUTO_CD              # 'dir' instead of 'cd dir'
setopt EXTENDED_HISTORY     # Save timestamp and duration
setopt APPEND_HISTORY       # Append rather than overwrite history
setopt INC_APPEND_HISTORY   # Add commands as they are typed, not at shell exit
setopt HIST_EXPIRE_DUPS_FIRST  # Expire duplicates first
setopt HIST_IGNORE_DUPS     # Do not store duplications
setopt HIST_FIND_NO_DUPS    # Ignore duplicates when searching
setopt HIST_REDUCE_BLANKS   # Remove blank lines from history

# Basic completion system
autoload -Uz compinit
compinit

# Simple but informative prompt
autoload -Uz colors && colors
PROMPT='%F{blue}%n%f@%F{green}$(scutil --get LocalHostName)%f %F{yellow}%~%f %# '

# Git status in right prompt (minimal)
autoload -Uz vcs_info
precmd() { vcs_info }

zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:*' stagedstr ' +'    # Symbol for staged changes
zstyle ':vcs_info:*' unstagedstr ' *'  # Symbol for unstaged changes
zstyle ':vcs_info:git:*' formats '%F{red}(%b%u%c)%f'
zstyle ':vcs_info:git:*' actionformats '%F{red}(%b|%a%u%c)%f'

setopt PROMPT_SUBST  # Enable variable substitution in prompts
RPROMPT='${vcs_info_msg_0_}'

# Essential aliases
alias ls='ls -G'
alias ll='ls -lah'

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

# Path settings
export PATH="/opt/homebrew/bin:$PATH"