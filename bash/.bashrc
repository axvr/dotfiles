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

# .NET Core development
export ASPNETCORE_ENVIRONMENT=Development
export DOTNET_CLI_TELEMETRY_OPTOUT=1


# ========================================
# --------- Custom Bash Prompts ----------
# ========================================

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

alias ledger="ledger -f ${HOME}/.ledger/personal.dat"
alias ledger-record="${EDITOR} ${HOME}/.ledger/personal.dat"
