#!/usr/bin/env bash

# Set up a new Fedora Linux install
#
# Fully update system before running this script

# ------------------------------------------------------------------------------
# Fedora Broadcom WiFi drivers:
# https://ashhar24.wordpress.com/2012/06/15/setting-up-wireless-driver-fedora/
# ------------------------------------------------------------------------------


y_n() {
    unset check
    read -rp "$1 [y/N]: " check
    case "$check" in
        [yY])   echo " ";;
        *)      return 0;;
    esac
}

case "$DESKTOP_SESSION" in
    "gnome") WM="GNOME";;
    "/usr/share/xsessions/plasma") WM="Plasma";;
esac


echo "========================================"
echo "---------- Configure System ------------"
echo "========================================"

# Configure GNOME
if [ "$WM" = "GNOME" ] && [ "$(y_n "Configure GNOME?")" ]; then
    wallpaper="file:///usr/share/backgrounds/gnome/adwaita-timed.xml"
    gsettings set org.gnome.desktop.interface gtk-theme     "Adwaita-dark"
    gsettings set org.gnome.desktop.background picture-uri  "$wallpaper"
    gsettings set org.gnome.desktop.screensaver picture-uri "$wallpaper"
    sudo dnf remove -y gnome-photos gnome-documents
    sudo dnf install -y shotwell gnome-tweaks
    # TODO set caps to ctrl, set resolution, set application folders
fi

# Configure KDE Plasma
if [ "$WM" = "Plasma" ]; then
    # TODO create alternative set up route for KDE Plasma
    # * Browser integration
    # * KDE connect
    # * Nextcloud support
    # * Latte dock
    # * Virtual box
    # * Set to defaults/auto-customise desktop
    true;
fi

# Set Hostname
if [ "$(y_n "Set hostname?")" ]; then
    read -rp "New Hostname (arctic):" hostname
    hostname="$(echo "$hostname" | sed 's/^$/arctic/')"
    hostname "$hostname"
fi


echo "========================================"
echo "-------- Install Applications ----------"
echo "========================================"

# Add RPMFusion free repo
if [ ! -f "/etc/yum.repos.d/rpmfusion-free.repo" ] && [ "$(y_n "Configure RPMFusion?")" ]; then
    sudo dnf install -y "https://download1.rpmfusion.org/free/fedora/rpmfusion-free-release-$(rpm -E %fedora).noarch.rpm"
    if [ "$(y_n "Install additional media codecs?")" ]; then
        sudo dnf install -y ffmpeg flac
        # TODO add more media codecs (e.g. mp4)
    fi
fi

# Install core applications
if [ "$(y_n "Install core applications?")" ]; then
    sudo dnf install -y keepassxc krita ledger unzip asunder firefox
fi

# Install other applications
if [ "$(y_n "Install other applications?")" ]; then
    sudo dnf install -y remmina electrum torbrowser-launcher gnome-usage \
        logisim youtube-dl pandoc seahorse
fi


echo "========================================"
echo "------ Install Development Tools -------"
echo "========================================"

# Install core development tools
if [ "$(y_n "Install core development tools?")" ]; then
    sudo dnf install -y git stow tmux vim ctags mercurial bash
fi

# Install other development tools
if [ "$(y_n "Install other development tools?")" ]; then
    sudo dnf install -y gettext doxygen subversion cvs mercurial #diff patch
    sudo dnf groupinstall -y "Development Tools"
fi

# Install other editors
if [ "$(y_n "Install other editors?")" ]; then
    # Install VS Code
    if [ "$(y_n "Visual Studio Code")" ]; then
        sudo rpm --import https://packages.microsoft.com/keys/microsoft.asc
        sudo sh -c 'echo -e "[code]\nname=Visual Studio Code\nbaseurl=https://packages.microsoft.com/yumrepos/vscode\nenabled=1\ngpgcheck=1\ngpgkey=https://packages.microsoft.com/keys/microsoft.asc" > /etc/yum.repos.d/vscode.repo'
        sudo dnf install -y code
    fi

    # Install Spacemacs
    if [ "$(y_n "Spacemacs")" ]; then
        sudo dnf install -y emacs
        if [ "$(y_n "Use development version of Spacemacs?")" ]; then
            git clone -b develop https://github.com/syl20bnr/spacemacs ~/.emacs.d
        else
            git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d
        fi
        # TODO install Source Code Pro font
    fi
fi

if [ "$(y_n "Install language specific tools?")" ]; then

    # Shell & Bash Scripting
    [ "$(y_n "Shell & Bash")" ] && sudo dnf install -y ShellCheck

    # Perl Scripting
    [ "$(y_n "Perl")" ] && sudo dnf install -y perl perl-CPAN

    # Ruby Scripting
    [ "$(y_n "Ruby")" ] && sudo dnf install -y ruby

    # LaTeX Typesetting
    if [ "$(y_n "LaTeX Typesetting")" ]; then
        # PDFLaTeX  (texlive-scheme-basic)
        # LaTeXmk   (latexmk)
        sudo dnf install -y texlive-scheme-basic latexmk \
            texlive-titling texlive-titlesec \
            texlive-roboto texlive-noto
    fi

    # JavaScript & TypeScript
    if [ "$(y_n "JavaScript & TypeScript")" ]; then
        sudo dnf install -y nodejs npm
        sudo npm install -g typescript
        [ "$(y_n "Vue.js")" ] && sudo npm install -g @vue/cli
        [ "$(y_n "Angular")" ] && sudo npm install -g @angular/cli
    fi

    # .NET Core SDK
    if [ "$(y_n ".NET Core")" ]; then
        sudo dnf copr enable @dotnet-sig/dotnet
        sudo dnf install -y dotnet-sdk-2.1 libuv libuv-devel
        dotnet tool install -g dotnet-watch dotnet-dev-certs dotnet-ef
    fi

    # Python
    if [ "$(y_n "Python")" ]; then
        sudo dnf install -y python python3 python2-devel python3-devel \
            python2-pip #python3-pip
    fi

    # Haskell
    [ "$(y_n "Haskell")" ] && sudo dnf install -y ghc-compiler

    # Clojure
    if [ "$(y_n "Clojure")" ]; then
        curl -O https://download.clojure.org/install/linux-install-1.9.0.381.sh
        chmod +x linux-install-1.9.0.381.sh
        sudo ./linux-install-1.9.0.381.sh
        rm linux-install-1.9.0.381.sh
    fi

    # Go
    [ "$(y_n "Go")" ] && sudo dnf install -y go

    # C
    if [ "$(y_n "C")" ]; then
        sudo dnf install -y make cmake autoconf automake gcc
        sudo dnf groupinstall -y "C Development Tools and Libraries"
    fi

    # TODO C++
    if [ "$(y_n "C++")" ]; then
        # TODO g++ clang-tidy
        sudo dnf install -y gcc-c++ clang gtkmm30-devel clang-tools-extra cppcheck

        # Bitcoin Core
        sudo dnf install -y libtool openssl-devel \
            libevent-devel boost-devel libdb4-devel libdb4-cxx-devel
        # Bitcoin Qt
        sudo dnf install -y miniupnpc-devel qt5-qttools-devel qt5-qtbase-devel \
            protobuf-devel qrencode-devel
    fi

    # TODO Ethereum

    # TODO Other tools
    # docker gtk+ qt (split from Bitcoin) libvtemm-devel
    #
    # Plan 9 User Space & Suckless tools
    # sudo dnf install -y libXt-devel

fi


echo "========================================"
echo "-------- System Configuration ----------"
echo "========================================"

# TODO install dotfiles?
# TODO install fonts (use custom font installer)

# TODO Create basic directory structure
# mkdir -p ~/Documents/{Projects,Notes,Backups} ~/.ledger

# Generate SSH keys
[ "$(y_n "Generate SSH keys?")" ] && ssh-keygen -t rsa -b 4096


# TODO install applications from source
