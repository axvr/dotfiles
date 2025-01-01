[[ $- != *i* ]] && return            # Abort if not running interactively
[ -f /etc/bashrc ] && . /etc/bashrc  # Source global definitions

export EDITOR=nvim VISUAL=nvim
export MANPAGER="less --RAW-CONTROL-CHARS --use-color --color=d+y --color=u+R"

export HISTSIZE=10000 HISTFILESIZE=1000000 HISTCONTROL=ignoreboth:erasedups
shopt -s histappend globstar checkwinsize

if (($COLUMNS >= 60)) && test "$(command -v git)"; then
    git_branch() { git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/^\*//'; }
    PS1="[\u@\h \[\e[0;34m\]\W\[\e[0;32m\]\`git_branch\`\[\e[0;00m\]]\$ "
elif (($COLUMNS >= 40)); then
    PS1="[\u@\h \[\e[0;34m\]\W\[\e[0;00m\]]\$ "
else
    PS1='$ '
fi
PS2="> "

[[ -s "$HOMEBREW_PREFIX/etc/profile.d/bash_completion.sh" ]] \
    && . "$HOMEBREW_PREFIX/etc/profile.d/bash_completion.sh"

# ---------------------------------

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
