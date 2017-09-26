#!/usr/bin/env bash
# -------------------------
# Manage Dotfiles
# -------------------------

# Install dots on your system:
# $ wget https://raw.githubusercontent.com/axvr/dotfiles/master/manage/dots.sh -O /usr/local/bin/dots.sh
# $ chmod u+x /usr/local/bin/dots.sh
# Or just run the downloaded dots.sh file


# Set location to store dotfiles
dots_location="$HOME/Documents/Projects/dotfiles"
mkdir -p "$dots_location"


function load_dotfiles() {
    return 0
}


function update_dotfiles() {
    return 0
}


function create_template {
    template="#!/usr/bin/env bash

function update() {
    # Commands to run to copy your dotfiles to the repo
    return 0
}

function install() {
    # Commands to run to copy your dotfiles from the repo
    return 0
}

# Do not modify the following code
case \$1 in
    -u|--update)
        update
        ;;
    -i|--install)
        install
        ;;
    *)
        return 1
        ;;
esac
"

    echo "$template" > .dot.sh

}

create_template

function print_message() {
    usage_message="usage: dots <option> [dotfile name]"

    if [ "$1" = "help" ]
    then
        printf "
Simple Dotfile Management Script:

%s

Options
    -h, --help      Display this help message.
\n" "$usage_message"
    elif [ "$1" = "usage" ]
    then
        echo "$usage_message"
    else
        return 1
    fi
}


case $1 in 
    -h|--help)
        print_message "help"
        ;;
    *)
        print_message "usage"
        ;;
esac

# vim: set ts=8 sw=4 tw=80 et ft=sh fdm=marker fmr={{{,}}} :
