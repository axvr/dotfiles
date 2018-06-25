#!/bin/sh

if [ -f ~/.kshrc ]; then
    . ~/.kshrc
fi

PATH=$PATH:$HOME/.local/bin:$HOME/bin
export PATH

# Set keyboard layout to UK
[ "$(command -v 'loadkeys')" ] && loadkeys gb
