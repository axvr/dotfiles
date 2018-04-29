#!/bin/sh

if [ -f "$HOME/.Xresources" ]; then
    xrdb "$HOME/.Xresources"
fi

setxkbmap gb
exec dwm
