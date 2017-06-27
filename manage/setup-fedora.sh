#!/usr/bin/env bash
# -----------------------------
# Set up Fedora
# -----------------------------

# Run this script
# wget https://https://raw.githubusercontent.com/axvr/dotfiles/master/manage/setup-fedora.sh && ./setup-fedora.sh

# Fedora Broadcom WiFi drivers
# https://ashhar24.wordpress.com/2012/06/15/setting-up-wireless-driver-fedora/

# Upgrade Fedora
sudo dnf -y upgrade

# Install Applications
sudo dnf -y install keepassxc polari gnome-tweak-tool asunder \
     gnome-todo libreoffice torbrowser-launcher

# Install Development Tools (Some may need to be installed via pip)
sudo dnf -y install emacs vim neovim \
     texlive-scheme-basic texlive-titling texlive-titlesec texlive-roboto \
     python python3 pylint python3-pylint python-nose python3-nose \
     python2-devel python3-devel python2-flake8 python3-flake8 \
     rust cargo \
     cppcheck clang gtkmm30-devel \
     cmake ctags shellcheck perl
cargo install rustfmt racer # TODO set up rust racer

sudo dnf -y groupinstall "Development Tools" \
     "C Development Tools and Libraries" \
     "GNOME Software Development"

# Install Fonts
sudo dnf -y install adobe-source-code-pro-fonts \
     google-roboto-fonts google-roboto-mono-fonts
# TODO install Iosevka font

# Install Themes
sudo dnf -y install arc-theme breeze-cursor-theme
mkdir -p ~/.fonts/Arc
git clone https://github.com/horst3180/arc-icon-theme.git /tmp/arc-temp
mv /tmp/arc-temp/Arc/* ~/.fonts/Arc/

# Install GNOME Extensions
mkdir -p ~/.local/share/
git clone https://github.com/rockon999/dynamic-panel-transparency.git \
    /tmp/dpt
mv /tmp/dpt/dynamic-panel-transparency@rockon999.github.io \
   ~/.local/share/gnome-shell/extensions/

# Set up Development Tools
mkdir -p ~/.emacs.d ~/.config/nvim
github=https://raw.githubusercontent.com/axvr/dotfiles/master
wget $github/spacemacs/spacemacs -O ~/.spacemacs
wget $github/neovim/init.vim     -O ~/.config/nvim/init.vim
wget $github/vim/vimrc           -O ~/.vimrc
wget $github/git/gitconfig       -O ~/.gitconfig
wget $github/bash/bashrc         -O ~/.bashrc
source ~/.bashrc
git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d

# TODO maybe do ssh key gen (ssh-keygen -t rsa -b 4096 -C "email@here.com")
