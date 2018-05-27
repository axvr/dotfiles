#!/usr/bin/env bash
# -----------------------------
# Set up Fedora
# -----------------------------

# Run install script using this command
# curl https://raw.githubusercontent.com/axvr/dotfiles/master/manage/fedora.sh | bash

# Fedora Broadcom WiFi drivers
# https://ashhar24.wordpress.com/2012/06/15/setting-up-wireless-driver-fedora/

# ------------------------------------------------------------------------------


# Upgrade Fedora System
sudo dnf -y upgrade

# Create basic directory structure
mkdir -p ~/Documents/{Projects,Notes}


# ========================================
# -------- Install Applications ----------
# ========================================

# TODO setup RPMFusion repos
# TODO install ffmpeg and cmus
# TODO install abcde & cdparanoia

sudo dnf -y install keepassxc krita torbrowser-launcher ledger gnome-tweaks
sudo dnf -y install youtube-dl weechat

sudo dnf copr enable fszymanski/newsboat
sudo dnf install newsboat


# ========================================
# ------ Install Development Tools -------
# ========================================

# Main Tools
sudo dnf -y install stow tmux vim ctags nvi

# Development Package Groups
sudo dnf -y groupinstall "Development Tools" \
    "C Development Tools and Libraries" \
    "GNOME Software Development"

# Shell & Bash Scripting
#   ShellCheck  (ShellCheck)
sudo dnf -y install ShellCheck

# Perl & Ruby Scripting
sudo dnf -y install perl perl-CPAN ruby

# LaTeX Typesetting
#   PDFLaTeX    (texlive-scheme-basic)
#   LaTeXmk     (latexmk)
sudo dnf -y install texlive-scheme-basic latexmk \
    texlive-titling texlive-titlesec \
    texlive-roboto texlive-noto

# Angular & Node.JS
sudo dnf install nodejs npm
sudo npm install -g @angular/cli


# ========================================
# -------- Install .NET Dev Tools --------
# ========================================

# .NET Core SDK
sudo dnf copr enable @dotnet-sig/dotnet
sudo dnf install dotnet-sdk-2.1

# OmniSharp
# TODO download omnisharp-http for linux x64 (Vim)
# TODO download omnisharp for linux x64 (Emacs)

# Visual Studio Code
#sudo rpm --import https://packages.microsoft.com/keys/microsoft.asc
#sudo sh -c 'echo -e "[code]\nname=Visual Studio Code\nbaseurl=https://packages.microsoft.com/yumrepos/vscode\nenabled=1\ngpgcheck=1\ngpgkey=https://packages.microsoft.com/keys/microsoft.asc" > /etc/yum.repos.d/vscode.repo'
#sudo dnf check-update
#sudo dnf install code



# C

# Plan 9 User Space
sudo dnf install libXt-devel

# C++
#cppcheck clang gtkmm30-devel clang-tools-extra

# Bitcoin (& C++)
sudo dnf install gcc-c++ libtool make autoconf automake openssl-devel \
    libevent-devel boost-devel libdb4-devel libdb4-cxx-devel python3
sudo dnf install miniupnpc-devel qt5-qttools-devel qt5-qtbase-devel \
    protobuf-devel qrencode-devel


# Python
#python python3 pylint python3-pylint python-nose python3-nose
#python2-devel python3-devel python2-flake8 python3-flake8


# docker

#cmake ctags
#gtk+ libvtemm-deve


# Install Font Packs
sudo dnf -y install google-roboto-fonts google-roboto-mono-fonts \
    google-noto-fonts-common google-noto-mono-fonts


# Generate SSH keys
ssh_key_gen() {
    echo "Creating SSH Key Pair"
    printf "Input Email address: "
    read -r email
    printf "\n"
    ssh-keygen -t rsa -b 4096 -C "$email"
    eval "$(ssh-agent -s)"
    ssh-add ~/.ssh/id_rsa
    cat ~/.ssh/id_rsa.pub
    echo "Add key to accounts"
}


install_games() {
    # Install Dwarf Fortress - https://www.acm.jhu.edu/~bjr/pages/dwarf-fortress-for-fedora.html
    wget -P /etc/yum.repos.d/ https://www.acm.jhu.edu/~bjr/fedora/dwarffortress/dwarffortress.repo
    sudo dnf install dwarffortress
    # TODO Install Nethack
}

# Window manager brightness control
#sudo dnf install brightnessctl
