#!/usr/bin/env bash
# ------------------------------
# ~/.bash_profile
# ------------------------------


# Get the aliases and functions
if [ -f ~/.bashrc ]; then
    . ~/.bashrc
fi

# User specific environment and startup programs

PATH=$PATH:$HOME/.local/bin:$HOME/bin
export PATH

# Start ssh-agent
[ "$(command -v ssh-agent)" ] && eval "$(ssh-agent -s)"

# Set keyboard layout to UK
[ "$(command -v loadkeys)" ] && loadkeys gb

# Start window manager on login
if [ -z "$DISPLAY" ] && [[ "$(tty)" = /dev/tty1 ]]; then
    #exec sway
    exec startx
    vlock
    true
fi
