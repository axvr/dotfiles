#!/bin/sh

# Start ssh-agent
[ "$(command -v ssh-agent)" ] && eval `ssh-agent`

# Set up touchpad
for i in $(xinput list | grep -i "\(touch\|track\) \?pad" | sed 's/^.*id=\([0-9]\+\).*$/\1/')
do
    tapping=$(xinput list-props $i | grep -i "tapping enabled" \
        | grep -vi "default" | sed 's/^.*(\([0-9]\+\)).*$/\1/')
    natural=$(xinput list-props $i | grep -i "natural scrolling enabled" \
        | grep -vi "default" | sed 's/^.*(\([0-9]\+\)).*$/\1/')
    echo "xinput set-prop $i $tapping 1"
    echo "xinput set-prop $i $natural 1"
done
exit

# All options for 'setxkbmap' are here: '/usr/share/X11/xkb/rules/base.lst'
setxkbmap -layout 'gb,us' -option 'ctrl:nocaps,grp:win_space_toggle'

# TODO improve this (also create the 1600x900 resolution if it doesn't exist)
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
