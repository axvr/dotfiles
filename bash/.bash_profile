#!/usr/bin/env bash

# ~/.bash_profile

# Get the aliases and functions
[ -f $HOME/.bashrc ] && . $HOME/.bashrc

# User specific environment and startup programs

# Plan 9 from User Space
if [ -d "$HOME/Documents/Projects/plan9port/" ]
then
    export PLAN9=$HOME/Documents/Projects/plan9port
else
    export PLAN9=$HOME/code/plan9port
fi

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
