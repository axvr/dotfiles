[[ $- != *i* ]] && return            # Abort if not running interactively
[ -f /etc/bashrc ] && . /etc/bashrc  # Source global definitions

export EDITOR=nvim VISUAL=nvim
export MANPAGER="less --RAW-CONTROL-CHARS --use-color --color=d+y --color=u+R"

export HISTSIZE=10000 HISTFILESIZE=1000000 HISTCONTROL=ignoreboth:erasedups
shopt -s histappend globstar checkwinsize

# ---------------------------------

PROMPT_COMMAND=()

if [ "$(command -v git)" ]; then
    ps1_git_head() { GIT_HEAD="$(git branch --contains HEAD --no-color 2> /dev/null \
        | sed -e '/^[^*]/d' -e 's/^\* //')"; }
    PROMPT_COMMAND+=('ps1_git_head')
fi
ps1_git() { [ -n "$GIT_HEAD" ] && printf "\e[0;32m%s " "$GIT_HEAD"; }

# Display SSH user and host info when SSH'd into a machine with this config.
[ -n "$SSH_CLIENT" ] && PROMPT_COMMAND+=('printf "\e[0;31m[%s@%s] " "$USER" "$HOSTNAME"')

# Dynamic length prompt.
ps1_dynamic() {
    if (($COLUMNS >= 60)); then
        PROMPT_DIRTRIM=2
        PS1="\[\e[0;34m\]\w \`ps1_git\`\[\e[0;00m\]\$ "
    elif (($COLUMNS >= 40)); then
        PROMPT_DIRTRIM=1
        PS1="\[\e[0;34m\]\w \[\e[0;00m\]\$ "
    else
        PS1="\[\e[0;00m\]\$ "
    fi
}

PROMPT_COMMAND+=('ps1_dynamic')
PS1="\[\e[0;00m\]\$ "
PS2="\[\e[0;00m\]> "

# ---------------------------------

[[ -s "$HOMEBREW_PREFIX/etc/profile.d/bash_completion.sh" ]] \
    && . "$HOMEBREW_PREFIX/etc/profile.d/bash_completion.sh"

alias ex='nvim -E'
alias vi='nvim'
alias vim='nvim'
alias svim='nvim -S Session.vim'
alias vimdiff='nvim -d'

alias ls='ls --color=auto'
alias grep='grep --color=auto'
alias egrep='egrep --color=auto'
alias fgrep='fgrep --color=auto'
alias ip='ip --color=auto'
alias diff='diff --color=auto'

export LEDGER_FILE="$HOME/Documents/Ledger/Personal.journal"
alias ledger='hledger'
alias ledger-record='$EDITOR "$LEDGER_FILE"'
alias ledger-web='echo "http://127.0.0.1:5000" | open-urls && hledger web -- --serve'

export NOTES_DIR="$HOME/Documents/Notes"
alias notes='nvim +Notes +only'
alias journal='nvim +Journal +only'

alias asdf='mise'

# Aliases to fix files on Apple computers.
alias unquarantine='xattr -d com.apple.quarantine'
alias fix_dir='chmod a=rx,u=rwx'
alias fix_file='chmod a=r,u=rw'
