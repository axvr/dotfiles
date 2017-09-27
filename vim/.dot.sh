#!/usr/bin/env bash

# Set location to store dotfiles
dotfile_location="$HOME/Documents/Projects/dotfiles/vim"

function update() {
    # Commands to run to copy your dotfiles to the repo
    cp "$HOME/.vim/vimrc" "$dotfile_location/vimrc"
    return 0
}

function install() {
    # Commands to run to copy your dotfiles from the repo
    # TODO check OS to install programs
    echo "Installing Vim"
    sudo dnf install vim
    # TODO install tools
    mkdir -p "$HOME/.vim"
    if [ ! -d "$HOME/.vim/pack/vivid/opt/Vivid.vim" ]
    then
        echo "Installing Vivid.vim"
        git clone https://github.com/axvr/Vivid.vim ~/.vim/pack/vivid/opt/Vivid.vim
    fi
    echo "Loading Vim dotfiles"
    cp "$dotfile_location/vimrc" "$HOME/.vim/vimrc"
    echo "Finished installing Vim"
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

