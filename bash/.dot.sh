#!/usr/bin/env bash

# Set location to store dotfiles
dotfile_location="$HOME/Documents/Projects/dotfiles/bash"

function update() {
    # Commands to run to copy your dotfiles to the repo
    cp "$HOME/.bashrc" "$dotfile_location/bashrc"
    return 0
}

function install() {
    # Commands to run to copy your dotfiles from the repo
    echo "Loading 'bashrc'"
    cp "$dotfile_location/bashrc" "$HOME/.bashrc"
    echo "Set up bash (restart shell or source ~/.bashrc)"
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

