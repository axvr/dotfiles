#!/usr/bin/env bash

# Set location to store dotfiles
dotfile_location="$HOME/Documents/Projects/dotfiles/sway"

function update() {
    # Commands to run to copy your dotfiles to the repo
    return 0
}

function install() {
    # Commands to run to copy your dotfiles from the repo
    return 0
}

# Do not modify the following code
case $1 in
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

