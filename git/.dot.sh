#!/usr/bin/env bash

# Set location to store dotfiles
dotfile_location="$HOME/Documents/Projects/dotfiles/git"

function update() {
    # Commands to run to copy your dotfiles to the repo
    cp "$HOME/.gitconfig" "$dotfile_location/gitconfig"
    cp "$HOME/.gitignore_global" "$dotfile_location/gitignore_global"
    return 0
}

function install() {
    # Commands to run to copy your dotfiles from the repo
    echo "Installing Git"
    # TODO check OS for installation of git
    sudo dnf install git
    echo "Loading Git dotfiles"
    cp "$dotfile_location/gitconfig" "$HOME/.gitconfig"
    cp "$dotfile_location/gitignore_global" "$HOME/.gitignore_global"
    echo "Finished intalling Git"
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

