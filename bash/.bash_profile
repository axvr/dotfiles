#!/usr/bin/env bash

# ~/.bash_profile

# Get the aliases and functions
if [ -f ~/.bashrc ]; then
    . ~/.bashrc
fi

# User specific environment and startup programs

PATH=$PATH:$HOME/.local/bin:$HOME/bin:$HOME/.dotnet/tools
export PATH

# TODO Start ssh-agent
# [ "$(command -v ssh-agent)" ] && eval "$(ssh-agent -s)"
