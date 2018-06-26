#!/usr/bin/env bash

# ========================================
# ------------- ~/.bashrc ----------------
# ========================================

# Source global definitions
if [ -f /etc/bashrc ]; then
    . /etc/bashrc
fi

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# History config
export HISTSIZE=1000
export HISTFILESIZE=2000
export HISTCONTROL=ignoredups:erasedups
shopt -s histappend

# Locale & keyboard config
export LANG=en_GB.UTF-8

# Bash Inprovements
shopt -s globstar
shopt -s checkwinsize


# ========================================
# --------- Development Config -----------
# ========================================

# Set default editor to Vim
VISUAL=vim
export VISUAL EDITOR=vim
export EDITOR

# Set Man pager
export PAGER="less"
export MANPAGER="less"
export MANWIDTH=80

# .NET Core development
export ASPNETCORE_ENVIRONMENT=Development
export DOTNET_CLI_TELEMETRY_OPTOUT=1


# ========================================
# --------- Custom Bash Prompts ----------
# ========================================

# Fedora Default
#export PS1="[\u@\h \W]\\$ "

# Fedora Default with VCS branch
function get_vcs_branch() {
    if [ "$(command -v vcs)" ]; then
        VCS_BRANCH=$(vcs -b)
        if [ "$VCS_BRANCH" != "" ]; then
            printf " [32m%s[00m" "$VCS_BRANCH"
        fi
    fi
    printf ""
}
export PS1="[\u@\h \W\`get_vcs_branch\`]\\$ "

# Termux (Android) version of Fedora default
[ "$(uname -o)" == "Android" ] && export PS1="[\W]\\$ "


# ========================================
# -------- Aliases and Functions ---------
# ========================================

alias ls="ls --color=auto"
alias grep="grep --color=auto"
alias egrep="egrep --color=auto"
alias fgrep="fgrep --color=auto"

alias ledger="ledger -f ${HOME}/.ledger/personal.dat"
alias ledger-record="${EDITOR} ${HOME}/.ledger/personal.dat"
