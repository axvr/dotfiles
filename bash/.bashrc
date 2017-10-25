#!/usr/bin/env bash
# -----------------------------
# ~/.bashrc
# -----------------------------
# Bash Configuration File
# -----------------------------


#################################
##### Initial Configuration #####
#################################

# Source global definitions
if [ -f /etc/bashrc ]; then
    . /etc/bashrc
fi

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Use 256 Colour
export TERM=xterm-256color

# History config
export HISTSIZE=10000
export HISTFILESIZE=20000
export HISTCONTROL=ignoredups:erasedups
shopt -s histappend
export PROMPT_COMMAND="${PROMPT_COMMAND:+$PROMPT_COMMAND$'\n'}history -a; history -c; history -r"

# Locale
export LANG=en_GB.UTF-8
export XKB_DEFAULT_LAYOUT=gb


#################################
###### Development Config #######
#################################

# Set default editor to Vim
VISUAL=vim
export VISUAL EDITOR=vim
export EDITOR

# Set Man pager
export MANPAGER="env MAN_PN=1 vim -M +MANPAGER -"

# Rust development
#export PATH=$PATH:$HOME/.cargo/bin


#################################
###### Custom Bash Prompts ######
#################################

# Default
#export PS1="[\u@\h \W]\\$ "

# Default with Git branch
function parse_git_branch() {
    BRANCH=$(git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/*\(.*\)/\1/')
    if [[ ! "${BRANCH}" == "" ]]; then
        echo "${BRANCH}"
    else
        echo ""
    fi
}
export PS1="[\u@\h \W\[\e[32m\]\`parse_git_branch\`\[\e[m\]]\\$ "

# Termux (Android) version of default
#export PS1="[\W]$ "


#################################
##### Aliases and Functions #####
#################################

alias startx='startx; vlock'
alias nv="nvim -u ~/.vim/vimrc"
