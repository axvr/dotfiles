#!/bin/sh

# TODO start ssh-agent

# All options for 'setxkbmap' are here: '/usr/share/X11/xkb/rules/base.lst'
setxkbmap -layout 'gb,us' -option 'ctrl:nocaps,grp:win_space_toggle'

# TODO improve this
xrandr --output eDP-1 --mode 1600x900

while true
do
    VOLUME="Vol: $(audio -v)%"
    DATETIME="$(date +%Y-%m-%d\ %H:%M)"
    BATTERY="$(power -s): $(power -c)%"
    xsetroot -name " $VOLUME | $DATETIME | $BATTERY "
    sleep 1s
done &

exec dwm
