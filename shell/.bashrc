#!/usr/bin/env bash

# ~/.bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
    . /etc/bashrc
fi

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# History config
export HISTSIZE=10000
export HISTFILESIZE=1000000
export HISTCONTROL=ignoreboth:erasedups
shopt -s histappend

# Bash enhancements
shopt -s globstar checkwinsize


# == Bash prompts ==

if (($COLUMNS >= 60)) && test "$(command -v git)"; then
    git_branch() { git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/^\*//'; }
    PS1="[\u@\h \W\[\e[0;32m\]\`git_branch\`\[\e[0;00m\]]\$ "
elif (($COLUMNS >= 40)); then
    PS1="[\u@\h \W]\$ "
else
    PS1="[\W]\$ "
fi

PS2="> "


# == Aliases & functions ==

alias vi="vim"
alias svim="vim -S Session.vim"

alias ls="ls --color=auto"
alias grep="grep --color=auto"
alias egrep="egrep --color=auto"
alias fgrep="fgrep --color=auto"

export LEDGER_FILE="$HOME/Documents/Ledger/Personal.journal"
alias ledger='hledger'
alias ledger-record='$EDITOR "$LEDGER_FILE"'
alias ledger-web='echo "http://127.0.0.1:5000" | open-urls && hledger web -- --serve'

alias fix_dir='chmod a=rx,u=rwx'
alias fix_file='chmod a=r,u=rw'
alias unquarantine='xattr -d com.apple.quarantine'

# This must be at the end of the file for SDKMAN! to work.
export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$SDKMAN_DIR/bin/sdkman-init.sh" ]] && . "$SDKMAN_DIR/bin/sdkman-init.sh"
