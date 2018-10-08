#!/bin/sh

# Configure an X session


# Set up touchpad
for i in $(xinput list | grep -i "\(touch\|track\) \?pad" | sed 's/^.*id=\([0-9]\+\).*$/\1/')
do
    tapping=$(xinput list-props "$i" | grep -i "tapping enabled" \
        | grep -vi "default" | sed 's/^.*(\([0-9]\+\)).*$/\1/')
    natural=$(xinput list-props "$i" | grep -i "natural scrolling enabled" \
        | grep -vi "default" | sed 's/^.*(\([0-9]\+\)).*$/\1/')
    xinput set-prop "$i" "$tapping" 1
    xinput set-prop "$i" "$natural" 1
done

# All options for 'setxkbmap' are here: '/usr/share/X11/xkb/rules/base.lst'
setxkbmap -layout 'gb,us' -option 'ctrl:nocaps,grp:win_space_toggle'

# TODO improve this (also create the 1600x900 resolution if it doesn't exist)
xrandr --output eDP-1 --mode 1600x900
# Virtual-1 <-- name of VM display


[ -z "$DESKTOP" ] && DESKTOP="dwm"
case "$DESKTOP" in
    dwm)
        battery() {
            state="$(power -s)"
            case "$state" in
                Charging)       state="+";;
                Discharging)    state="-";;
                Full)           state="" ;;
                *)              state="?";;
            esac
            printf "BAT %s%s%%" "$state" "$(power -c)"
        }

        while true
        do
            volume="VOL $(audio -v)%"
            datetime="$(date +%Y-%m-%d\ %H:%M)"
            xsetroot -name " [$volume] [$(battery)] [$datetime] "
            sleep 1s
        done &

        # FIXME get slock working
        xautolock -time 10 -locker slock

        exec dwm
        ;;
    i3)     ;;
    bspwm)  ;;
    plasma) exec startkde ;;
    gnome)  exec gnome-session ;;
esac
