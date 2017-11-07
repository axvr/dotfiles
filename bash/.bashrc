#!/usr/bin/env bash
# -----------------------------
# ~/.bashrc
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
export PAGER="less"
export MANPAGER="less"
#export MANPAGER="env MAN_PN=1 vim -M +MANPAGER -" # Vim as MANPAGER

# Rust development
#export PATH=$PATH:$HOME/.cargo/bin


#################################
###### Custom Bash Prompts ######
#################################

# Default
#export PS1="[\u@\h \W]\\$ "

# Default with VCS branch
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

# Termux (Android) version of default
#export PS1="[\W]$ "


#################################
##### Aliases and Functions #####
#################################

# FIXME Custom per project note management solution
export NOTE_DIRECTORY="${HOME}/Documents/Notes"
function Note() {
    if [[ ! -d "${NOTE_DIRECTORY}" ]]; then
        mkdir -p "${NOTE_DIRECTORY}"
    fi

    if [[ "$@" = "" ]]; then
        # below is pointless
        if [[ parse_vcs_branch != "" ]]; then
            NOTE_PROJECT="${PWD##*/}"
        fi
        $EDITOR "${NOTE_DIRECTORY}/Projects/${NOTE_PROJECT}.md"
    else
        $EDITOR "${NOTE_DIRECTORY}/${1}.md"
    fi
}

# TODO improve this
#function Notes() {
#    echo "~/Documents/Notes:"
#    ls -Ac ~/Documents/Notes/ | grep "$*"
#}

alias Notes="ls -A ~/Documents/Notes/"


alias startx='startx; vlock'
alias vi="vim"
alias nv="nvim -u ~/.vim/vimrc"
alias em="emacs -nw"
alias ledger="ledger -f ~/.ledger/personal.dat"
alias ledger-record="${EDITOR} ~/.ledger/personal.dat"
alias dl="youtube-dl"

