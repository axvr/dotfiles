#!/usr/bin/env bash

# ~/.bash_profile

# Get the aliases and functions
if [ -f ~/.bashrc ]; then
    . ~/.bashrc
fi

# User specific environment and startup programs

# Plan 9 User Space
export PLAN9=$HOME/Documents/Projects/plan9port
# export PLAN9=$HOME/code/plan9port

PATH=$PATH:$HOME/.local/bin:$HOME/bin:$PLAN9/bin:$HOME/.dotnet/tools
export PATH

# TODO Start ssh-agent
# [ "$(command -v ssh-agent)" ] && eval "$(ssh-agent -s)"

# Start window manager on login in TTY1
if [ -z "$DISPLAY" ] && [[ "$(tty)" = /dev/tty1 ]]; then
    exec startx
    # exec wayland
    vlock
fi
