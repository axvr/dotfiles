#!/bin/sh

# All options for 'setxkbmap' are here: '/usr/share/X11/xkb/rules/base.lst'
setxkbmap -layout 'gb,us' -option 'ctrl:nocaps,grp:win_space_toggle'

# TODO set screen resolution (laptop: 1600x900)

exec dwm
