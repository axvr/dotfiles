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

# Locale
export LANG=en_GB.UTF-8
export XKB_DEFAULT_LAYOUT=gb

# Set Caps Lock to Ctrl
export XKB_DEFAULT_OPTIONS=ctrl:nocaps


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

# Plan 9 User Space
if [ -d "$HOME/Documents/Projects/plan9/" ]; then
    export PLAN9=$HOME/Documents/Projects/plan9
    export PATH=$PATH:$PLAN9/bin
fi

# Rust development
#export PATH=$PATH:$HOME/.cargo/bin

# Microsoft VSTS-CLI Tool
if [ -d "$HOME/.vsts/" ]; then
    export PATH=$PATH:$HOME/.vsts/bin
    source "$HOME/.vsts/vsts.completion"
fi

# TFVC (TEE-CLC)
if [ -d "$HOME/.tee-clc/" ]; then
    export PATH=$PATH:$HOME/.tee-clc/
    export TF_AUTO_SAVE_CREDENTIALS="1"
    export TF_DIFF_COMMAND="diff %1 %2"
    #export TF_MERGE_COMMAND="vimdiff %1 %2 %4"
fi

# Set .NET Core environment variables
export ASPNETCORE_ENVIRONMENT=Development
export DOTNET_CLI_TELEMETRY_OPTOUT=1


# ========================================
# --------- Custom Bash Prompts ----------
# ========================================

# Fedora Default
#export PS1="[\u@\h \W]\\$ "

# Fedora Default with VCS branch
function parse_vcs_branch() {
    # Check Git Branch
    BRANCH=$(git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/*\(.*\)/\1/')
    # Check HG Branch
    if [[ "${BRANCH}" == "" ]]; then
        BRANCH=$(hg branch 2> /dev/null | awk '{print " "$1""}')
    fi
    printf "${BRANCH}"
}
export -f parse_vcs_branch
export PS1="[\u@\h \W\[\e[32m\]\`parse_vcs_branch\`\[\e[m\]]\\$ "

# Termux (Android) version of Fedora default
#export PS1="[\W]$ "


# ========================================
# -------- Aliases and Functions ---------
# ========================================

# TODO note and todo command
alias startx="startx; vlock"
alias nv="nvi"
alias em="emacs -nw"
alias irc="irssi"
alias ledger="ledger -f ${HOME}/.ledger/personal.dat"
alias ledger-record="${EDITOR} ${HOME}/.ledger/personal.dat"
alias dl="youtube-dl"
alias vi="vim -u NONE"

