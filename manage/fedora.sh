#!/usr/bin/env bash

# Set up a new Fedora Linux install

# ------------------------------------------------------------------------------
# Fedora Broadcom WiFi drivers:
# https://ashhar24.wordpress.com/2012/06/15/setting-up-wireless-driver-fedora/
# ------------------------------------------------------------------------------


# Upgrade Fedora System
sudo dnf -y upgrade

# Create basic directory structure
mkdir -p ~/Documents/{Projects,Notes}


# ========================================
# -------- Install Applications ----------
# ========================================

# Add RPMFusion free repo
sudo dnf install https://download1.rpmfusion.org/free/fedora/rpmfusion-free-release-$(rpm -E %fedora).noarch.rpm

sudo dnf -y install keepassxc krita torbrowser-launcher ledger gnome-tweaks
sudo dnf -y install youtube-dl weechat abcde ffmpeg cdparanoia cmus unzip

# FIXME for F28
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
sudo dnf -y install nodejs npm
sudo npm install -g @angular/cli

# .NET Core SDK
sudo dnf copr enable @dotnet-sig/dotnet
sudo dnf -y install dotnet-sdk-2.1

# Python
sudo dnf -y install python python3 python2-devel python3-devel

# C
sudo dnf -y install make cmake autoconf automake gcc

# Plan 9 User Space & Suckless tools
sudo dnf -y install libXt-devel

# C++
sudo dnf -y install gcc-c++ clang gtkmm30-devel clang-tools-extra cppcheck

# Bitcoin (& C++)
sudo dnf install libtool openssl-devel \
    libevent-devel boost-devel libdb4-devel libdb4-cxx-devel
sudo dnf install miniupnpc-devel qt5-qttools-devel qt5-qtbase-devel \
    protobuf-devel qrencode-devel

# Other tools
# docker gtk+ libvtemm-devel


# Generate SSH keys
ssh_key_gen() {
    ssh-keygen -t rsa -b 4096
    eval "$(ssh-agent -s)"
    ssh-add ~/.ssh/id_rsa
    cat ~/.ssh/id_rsa.pub
    echo "Add this key to your accounts"
}
