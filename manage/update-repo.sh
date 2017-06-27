#!/usr/bin/env bash

# Add dotfiles to the dotfiles repo

# Vim - vimrc
cp ~/.vimrc ~/Documents/Projects/dotfiles/vim/vimrc

# Neovim - init.vim
cp ~/.config/nvim/init.vim ~/Documents/Projects/dotfiles/neovim/init.vim

# Spacemacs - spacemacs
cp ~/.spacemacs ~/Documents/Projects/dotfiles/spacemacs/spacemacs

# Clang Format - clang-format
cp ~/Documents/Projects/mozilla/firefox/.clang-format ~/Documents/Projects/dotfiles/clang/clang-format
cp ~/Documents/Projects/mozilla/firefox/.clang-format-ignore ~/Documents/Projects/dotfiles/clang/clang-format-ignore
cp ~/Documents/Projects/mozilla/firefox/.clang-tidy ~/Documents/Projects/dotfiles/clang/clang-tidy

# Xresources - Xresources
cp ~/.Xresources ~/Documents/Projects/dotfiles/urxvt/Xresources

# Bash - bashrc
cp ~/.bashrc ~/Documents/Projects/dotfiles/bash/bashrc

# Sway - config
cp ~/.config/sway/config ~/Documents/Projects/dotfiles/sway/config

# Qutebrowser -
cp ~/.config/qutebrowser/* ~/Documents/Projects/dotfiles/qutebrowser/

# Git - gitconfig
cp ~/.gitconfig ~/Documents/Projects/dotfiles/git/gitconfig

