#!/usr/bin/env bash

# Set location to store dotfiles
dotfile_location="$HOME/Documents/Projects/dotfiles/spacemacs/"

function update() {
    # Commands to run to copy your dotfiles to the repo
    cp "$HOME/.spacemacs" "$dotfile_location/spacemacs"
    return 0
}

function install() {
    # Commands to run to copy your dotfiles from the repo
    echo "Installing Emacs and required tools"
    # TODO check OS for installation
    sudo dnf install emacs
    # TODO install tools for spacemacs
    echo "Removing '~/.emacs.d' directory"
    rm -rf "$HOME/.emacs.d"
    echo "Cloning Spacemacs Git repository"
    git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d
    echo "Loading Spacemacs dotfile"
    cp "$dotfile_location/spacemacs" "$HOME/.spacemacs"
    echo "Finished installing Spacemacs"
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

