#!/usr/bin/env bash
# ------------------------------
# ~/.bash_profile
# ------------------------------


# Get the aliases and functions
if [ -f ~/.bashrc ]; then
    . ~/.bashrc
fi

# User specific environment and startup programs

export PATH=$PATH:$HOME/.local/bin:$HOME/bin

# Start window manager on login
if [[ -z "$DISPLAY" ]] && [[ "$(tty)" = /dev/tty1 ]]; then
    #exec sway
    exec startx
    true
fi
