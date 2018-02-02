#!/usr/bin/env bash
# -----------------------------
# Set up Fedora
# -----------------------------

# Run install script using this command
# wget https://raw.githubusercontent.com/axvr/dotfiles/master/manage/fedora.sh && bash fedora.sh

# Fedora Broadcom WiFi drivers
# https://ashhar24.wordpress.com/2012/06/15/setting-up-wireless-driver-fedora/

# ------------------------------------------------------------------------------


# Upgrade Fedora System
sudo dnf -y upgrade



# ========================================
# -------- Install Applications ----------
# ========================================

# Currently in use:
#   KeePassXC       (keepassxc)
#   Krita           (krita)
#   TorBrowser      (torbrowser-launcher)
#   Ledger-CLI      (ledger)
#   GNOME Tweaks    (gnome-tweak-tool)
#   TODO Terminal based IRC client: IRSSI or WeeChat

# Installed by default:
#   Firefox         (firefox)
#   LibreOffice     (libreoffice)
#   Evolution       (evolution evolution-ews)

# Previously used:
#   Asunder         (asunder)
#   Hexchat         (hexchat)
#   Taskwarrior     (taskwarrior)
#   YouTube-Dl      (youtube-dl)


sudo dnf -y install keepassxc krita torbrowser-launcher ledger gnome-tweak-tool



# ========================================
# ------ Install Development Tools -------
# ========================================

# General
#   Vim         (vim)
#   Tmux        (tmux)
#   GNU Stow    (stow)
#   Ctags       (ctags)
# No longer used:
#   Neovim ???
#   GNU Emacs ??? (spacemacs)
#   nvi ???
#   Visual Studio Code ???
#   GNOME Builder ???
#   Qt Creator ???

sudo dnf -y install stow tmux vim ctags


# C#, F#, VB
#   .NET Core SDK & CLI
#   VSTS-CLI
#   mono ???

# .NET Core SDK
sudo dnf copr enable @dotnet-sig/dotnet
sudo dnf install dotnet-sdk-2.0
# VSTS-CLI
curl -L https://aka.ms/install-vsts-cli | bash



# C

# Plan 9 User Space
sudo dnf install libXt-devel


# C++


# Rust
#   Rust        (rust)
#   Cargo       (cargo)

sudo dnf -y install rust cargo
#cargo install rustfmt racer


# Perl
#   Perl        (perl)
#   CPAN        (perl-CPAN)

sudo dnf -y install perl perl-CPAN


# Python


# LaTeX
#   PDFLaTeX    (texlive-scheme-basic)
#   LaTeXmk     (latexmk)

sudo dnf -y install texlive-scheme-basic latexmk \
    texlive-titling texlive-titlesec \
    texlive-roboto texlive-noto





sudo dnf -y groupinstall "Development Tools" \
    "C Development Tools and Libraries" \
    "GNOME Software Development"


# Bitcoin (& C++)
sudo dnf install gcc-c++ libtool make autoconf automake openssl-devel \
    libevent-devel boost-devel libdb4-devel libdb4-cxx-devel python3
sudo dnf install miniupnpc-devel qt5-qttools-devel qt5-qtbase-devel \
    protobuf-devel qrencode-devel


# ShellCheck
# docker
# pandoc ???

    sudo dnf -y install nvi vim neovim emacs \
         texlive-scheme-basic texlive-titling texlive-titlesec \
         texlive-roboto texlive-noto latexmk \
         python python3 pylint python3-pylint python-nose python3-nose \
         python2-devel python3-devel python2-flake8 python3-flake8 \
         rust cargo \
         cppcheck clang gtkmm30-devel clang-tools-extra \
         cmake ctags ShellCheck perl perl-CPAN
         gtk+ libvtemm-devel


# Install Font Packs
    sudo dnf -y install adobe-source-code-pro-fonts \
         google-roboto-fonts google-roboto-mono-fonts \
         google-noto-fonts-common google-noto-mono-fonts


# Generate SSH keys
function ssh_key_gen() {
        printf "
        Input Email address: "
        read -r email
        printf "\n"
        ssh-keygen -t rsa -b 4096 -C "$email"
        eval "$(ssh-agent -s)"
        ssh-add ~/.ssh/id_rsa
        cat ~/.ssh/id_rsa.pub
        echo "Add key to accounts"
}


function install_games() {

  # Install Dwarf Fortress - https://www.acm.jhu.edu/~bjr/pages/dwarf-fortress-for-fedora.html
  wget -P /etc/yum.repos.d/ https://www.acm.jhu.edu/~bjr/fedora/dwarffortress/dwarffortress.repo
  sudo dnf install dwarffortress

}
