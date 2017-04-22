#!/usr/bin/env bash

# Fedora 25 setup script for Alex Vear

# Upgrade fedora (without prompt)
sudo dnf -y upgrade

# Setup version control
sudo dnf install git hg
git config --global user.name "Alex Vear"
git config --global user.email "axvr@bitmessage.ch"

# Create basic file system structure
mkdir -p ~/Documents/Projects/
mkdir -p ~/.vim/
mkdir -p ~/.config/nvim/
mkdir -p ~/.emacs.d/
echo "Setup SSH key pair and change git origins to SSH"
git clone https://github.com/axvr/dotfiles.git ~/Documents/Projects/dotfiles

# install and setup editors
sudo dnf install emacs vim neovim
bash ~/Documents/Projects/dotfiles/install.sh
git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d

# install email client
sudo dnf -y install thunderbird
sudo dnf -y remove evolution

# Programming utilities
# TODO setup for python3 instead of python2
sudo dnf -y install pylint rust perl ctags automake gcc gcc-c++ cmake
sudo dnf -y install python-devel python3-devel clang cargo python python3
sudo dnf -y install python-nose python3-nose python3-pylint
sudo dnf -y install texlive-scheme-basic
cargo install racer # TODO Needs sorting out
cargo install rustfmt

# Generate SSH key pair (Add to GitHub!)
ssh-keygen -t rsa -b 4096 -C "axvr@bitmessage.ch"

