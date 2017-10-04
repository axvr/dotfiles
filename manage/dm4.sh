#!/usr/bin/env bash
# -------------------------
# DM4 - Dotfile Manager v4
# -------------------------

# DM4 - the dotfile manger designed for ricers. (name is subject to change)

# TODO maybe create a config file for DM4 to use (dm4rc? location? how to mod dm4?)

















declare -i DM4_NUM_ARGS=$#
declare -a DM4_ARGUMENTS=($@)

#declare -s DM4_DIR_PATH="$HOME/Documents/dotfiles"
#if [ ! -d "$DM4_DIR_PATH" ]
#then
#    ask the user if the directory should be created
#    mkdir -p "$DM4_DIR_PATH"
#fi

# Check a command has been given
if [ "$DM4_NUM_ARGS" -le 0 ]
then
    printf "No command given. Try the 'help' command.\n"
    exit
fi

case ${DM4_ARGUMENTS[0]} in
    pull)
        echo "Pull stuff"
        ;;
    load)
        echo "Load something"
        ;;
    help|--help)
        echo "Help Me"
        ;;
    *)
        printf "No such command: '%s'. Try the 'help' command.\n" \
            "${DM4_ARGUMENTS[0]}"
        ;;
esac

exit

# vim: set ts=8 sw=4 tw=80 et ft=sh fdm=marker fmr={{{,}}} :
