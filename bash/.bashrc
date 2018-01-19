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

# Rust development
#export PATH=$PATH:$HOME/.cargo/bin

# Microsoft VSTS-CLI Tool
export PATH=$PATH:/home/axvr/.vsts-cli/bin
source '/home/axvr/.vsts-cli/vsts.completion'


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

alias startx="startx; vlock"
alias nv="nvim"
alias em="emacs -nw"
alias ledger="ledger -f ~/.ledger/personal.dat"
alias ledger-record="${EDITOR} ~/.ledger/personal.dat"
alias dl="youtube-dl"

