#!/usr/bin/env bash

#############################################
### Fedora 25 set up script for Alex Vear ###
#############################################


# to use this script:
# $ wget -O setup-fedora.sh https://raw.githubusercontent.com/axvr/dotfiles/master/setup-fedora.sh && ./setup-fedora.sh


##################
### Pre-set-up ###
##################

# Upgrade fedora (without prompt)
sudo dnf -y upgrade

# Set up Git version control
sudo dnf install git
git config --global user.name "Alex Vear"
git config --global user.email "axvr@bitmessage.ch"

# Create basic file system structure
mkdir -p ~/Documents/Projects/ ~/.vim/ ~/.config/nvim/ ~/.emacs.d/
echo "Change git origin to SSH"
git clone https://github.com/axvr/dotfiles.git ~/Documents/Projects/dotfiles


######################################
### Application Install and config ###
######################################

# install and set up editors
sudo dnf install emacs vim neovim
bash ~/Documents/Projects/dotfiles/install.sh
git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d

# Other applications
sudo dnf -y remove evolution
sudo dnf -y install keepassx hexchat gnome-tweak-tool asunder \
     torbrowser-launcher gnome-todo libreoffice thunderbird


##########################
### Development Set up ###
##########################

# LaTeX packages
sudo dnf -y install texlive-scheme-basic texlive-titling texlive-titlesec \
     texlive-roboto

# Generate SSH key pair (Add to GitHub!)
echo "Add SSH Key to GitHub account"
ssh-keygen -t rsa -b 4096 -C "axvr@bitmessage.ch"

# Programming utilities
sudo dnf -y install pylint rust perl clang cargo python python3 python-nose \
     cppcheck
cargo install racer # TODO Needs sorting out
cargo install rustfmt

# Set up system for Mozilla Development
wget -O bootstrap.py https://hg.mozilla.org/mozilla-central/raw-file/default/python/mozboot/bin/bootstrap.py
python bootstrap.py
rm bootstrap.py

# Servo development set up
# TODO Solve servo not building issue with openssl
sudo dnf install rpm-build python2-virtualenv ncurses-devel ncurses-c++-libs  \
     mesa-libOSMesa-devel mesa-libOSMesa mesa-libGLU-devel libXmu-devel gperf \
     gl-manpages freeglut-devel freeglut cabextract


##################################
### Other system configuration ###
##################################

# Install fonts and themes
sudo dnf -y install adobe-source-code-pro-fonts google-roboto-fonts \
     google-roboto-mono-fonts atc-theme breeze-cursor-theme

# Install Arc icon theme
mkdir -p ~/.icons/Arc/ ~/.icons/Arc-temp
wget -O ~/.icons/arc.tar.gz https://github.com/horst3180/arc-icon-theme/archive/20161122.tar.gz
tar zxvf ~/.icons/arc.tar.gz -C ~/.icons/Arc-temp/ && rm -r ~/.icons/arc.tar.gz
mv ~/.icons/Arc-temp/$(ls ~/.icons/Arc-temp/)/* ~/.icons/Arc-temp/
mv ~/.icons/Arc-temp/Arc/* ~/.icons/Arc/ && rm -r ~/.icons/Arc-temp/

# Gnome Extensions
git clone https://github.com/rockon999/dynamic-panel-transparency.git ~/gnome-ext-temp
mv ~/gnome-ext-temp/dynamic-panel-transparency@rockon999.github.io \
   ~/.local/share/gnome-shell/extensions/ && rm -r ~/gnome-ext-temp

