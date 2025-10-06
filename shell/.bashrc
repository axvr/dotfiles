# ~/.bashrc - Interactive Bash config.

[[ $- != *i* ]] && return            # Abort if not running interactively.
[ -f /etc/bashrc ] && . /etc/bashrc  # Source global definitions.

export HISTSIZE=10000 HISTFILESIZE=1000000 HISTCONTROL=ignoreboth:erasedups
shopt -s histappend globstar checkwinsize
stty -ixon  # Disable TTY start/stop control.  (CTRL-S / CTRL-Q)

# ---------------------------------

PROMPT_COMMAND=()

if [ "$(command -v git)" ]; then
    ps1_git_head() { PS1_GIT_HEAD="$(git branch-name)"; }
    PROMPT_COMMAND+=('ps1_git_head')
fi

ps1_dynamic() {
    # Display [user@host] when SSH'd into a machine with this config.
    [ -n "$SSH_CLIENT" ] && local ssh="\[\e[0;31m\][$USER@$HOSTNAME] "
    if (($COLUMNS >= 60)); then
        PROMPT_DIRTRIM=2
        # Display Git branch name.
        [ -n "$PS1_GIT_HEAD" ] && local branch="\[\e[0;32m\]$PS1_GIT_HEAD "
        PS1="$ssh\[\e[0;34m\]\w $branch\[\e[0;00m\]\$ "
    elif (($COLUMNS >= 40)); then
        PROMPT_DIRTRIM=1
        PS1="$ssh\[\e[0;34m\]\w \[\e[0;00m\]\$ "
    else
        PS1="$ssh\[\e[0;00m\]\$ "
    fi
}

PROMPT_COMMAND+=('ps1_dynamic')
PS1="\[\e[0;00m\]\$ "
PS2="\[\e[0;00m\]> "

# ---------------------------------

[ -s "$HOMEBREW_PREFIX/etc/profile.d/bash_completion.sh" ] \
    && . "$HOMEBREW_PREFIX/etc/profile.d/bash_completion.sh"

export EDITOR='nvim' VISUAL="$EDITOR"
alias ex='vim -E' vi='vim' svim='vim -S Session.vim'
[ "$EDITOR" = 'nvim' ] && alias vim="nvim" vimdiff='nvim -d'

alias ls='ls --color=auto'
alias ip='ip --color=auto'
alias diff='diff --color=auto'
alias grep='grep --color=auto' egrep='egrep --color=auto' fgrep='fgrep --color=auto'

export MANPAGER='less --RAW-CONTROL-CHARS --use-color --color=d+y --color=u+R'

export LEDGER_FILE="$HOME/Documents/Ledger/Personal.journal"
alias ledger='hledger'
alias ledger-record='$EDITOR "$LEDGER_FILE"'
alias ledger-web='echo "http://127.0.0.1:5000" | open-urls && hledger web -- --serve'

export NOTES_DIR="$HOME/Documents/Notes"
alias notes='vim +Notes +tabonly'

# Aliases to fix files on Apple computers.
alias unquarantine='xattr -d com.apple.quarantine'
alias fix_dir='chmod a=rx,u=rwx'
alias fix_file='chmod a=r,u=rw'
