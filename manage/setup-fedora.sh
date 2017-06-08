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
git config --global core.editor "vim"

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
sudo dnf -y install keepassx polari gnome-tweak-tool asunder \
     torbrowser-launcher gnome-todo libreoffice


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

# Set up system for Mozilla compatible Development
wget -O /tmp/bootstrap.py https://hg.mozilla.org/mozilla-central/raw-file/default/python/mozboot/bin/bootstrap.py
python /tmp/bootstrap.py

# Servo compatible development set up
sudo dnf install rpm-build python2-virtualenv ncurses-devel ncurses-c++-libs  \
     mesa-libOSMesa-devel mesa-libOSMesa mesa-libGLU-devel libXmu-devel gperf \
     gl-manpages freeglut-devel freeglut cabextract


##################################
### Other system configuration ###
##################################

# Install fonts and themes
sudo dnf -y install adobe-source-code-pro-fonts google-roboto-fonts \
     google-roboto-mono-fonts arc-theme breeze-cursor-theme

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

# Gnome shell set up
gnome-shell --replace
gnome-shell-extension-tool -e dynamic-panel-transparency@rockon999.github.io
gsettings set org.gnome.desktop.interface icon-theme "Arc"
gsettings set org.gnome.desktop.interface gtk-theme "Arc-Darker"
gsettings set org.gnome.shell.extensions.user-theme name "Arc-Dark"
gsettings set org.gnome.desktop.interface cursor-theme "Breeze_cursors"
echo "Enable Global Dark theme in Gnome Tweak Tool"
echo "Enable User-themes Extension in Gnome Tweak Tool"
echo "Gnome may require a restart and then enabling extensions if this was not successful"
echo "Set Dynamic panel transparency time to 200ms"
echo "Set up online accounts"
echo "Set up GitHub SSH Key and change dotfiles clone to origin to SSH address"
echo "Clone other project repos"

