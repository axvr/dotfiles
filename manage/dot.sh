#!/bin/env bash

mkdir -p ~/Documents/Projects/dotfiles/vim/

cp -r ~/.vim/vimrc ~/Documents/Projects/dotfiles/vim/


# Set up Development Tools
rm -rf ~/.emacs.d/
mkdir -p ~/.emacs.d ~/.config/nvim ~/.vim
github=https://raw.githubusercontent.com/axvr/dotfiles/master
wget $github/spacemacs/spacemacs -O ~/.spacemacs
#wget $github/neovim/init.vim     -O ~/.config/nvim/init.vim
wget $github/vim/vimrc           -O ~/.vim/vimrc
wget $github/git/gitconfig       -O ~/.gitconfig
wget $github/bash/bashrc         -O ~/.bashrc
source "/home/$USER/.bashrc"
git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d

# Install gnome-shell-extension-cl (original: https://github.com/cyberalex4life/gnome-shell-extension-cl)
sudo wget https://raw.githubusercontent.com/axvr/gnome-shell-extension-cl/master/gnome-shell-extension-cl \
    -O /usr/local/bin/gnome-shell-extension-cl
sudo chmod +x /usr/local/bin/gnome-shell-extension-cl
