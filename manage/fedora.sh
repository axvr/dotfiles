#!/usr/bin/env bash

# Set up a new Fedora Linux install
# Update system before running

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


echo "========================================"
echo "-------- Install Applications ----------"
echo "========================================"

# Add RPMFusion free repo
if [ "$(y_n "Configure RPMFusion?")" ]; then
    sudo dnf install https://download1.rpmfusion.org/free/fedora/rpmfusion-free-release-$(rpm -E %fedora).noarch.rpm
    if [ "$(y_n "Install additional media codecs?")" ]; then
        sudo dnf install -y ffmpeg flac
        # TODO add more media codecs (e.g. mp4)
    fi
fi

# Install core applications
if [ "$(y_n "Install core applications?")" ]; then
    sudo dnf -y install keepassxc krita ledger unzip gnome-tweaks asunder \
        firefox
fi

# Install other applications
if [ "$(y_n "Install other applications?")" ]; then
    sudo dnf -y install remmina electrum torbrowser-launcher gnome-usage \
        logisim youtube-dl pandoc
fi


echo "========================================"
echo "------ Install Development Tools -------"
echo "========================================"

# Install core development tools
if [ "$(y_n "Install core development tools?")" ]; then
    sudo dnf -y install git stow tmux vim ctags mercurial bash
fi

# Install other development tools
if [ "$(y_n "Install other development tools?")" ]; then
    sudo dnf -y install gettext doxygen subversion cvs mercurial diff patch
    sudo dnf -y groupinstall "Development Tools"
fi

# Install "modern" editors
if [ "$(y_n "Install \"modern\" editors?")" ]; then
    # Install VS Code
    sudo rpm --import https://packages.microsoft.com/keys/microsoft.asc
    sudo sh -c 'echo -e "[code]\nname=Visual Studio Code\nbaseurl=https://packages.microsoft.com/yumrepos/vscode\nenabled=1\ngpgcheck=1\ngpgkey=https://packages.microsoft.com/keys/microsoft.asc" > /etc/yum.repos.d/vscode.repo'
fi

if [ "$(y_n "Install language specific tools?")" ]; then

    # Shell & Bash Scripting
    [ "$(y_n "Shell & Bash")" ] && sudo dnf -y install ShellCheck

    # Perl Scripting
    [ "$(y_n "Perl")" ] && sudo dnf -y install perl perl-CPAN

    # Ruby Scripting
    [ "$(y_n "Ruby")" ] && sudo dnf -y install ruby

    # LaTeX Typesetting
    if [ "$(y_n "LaTeX Typesetting")" ]; then
        # PDFLaTeX  (texlive-scheme-basic)
        # LaTeXmk   (latexmk)
        sudo dnf -y install texlive-scheme-basic latexmk \
            texlive-titling texlive-titlesec \
            texlive-roboto texlive-noto
    fi

    # JavaScript & TypeScript
    if [ "$(y_n "JavaScript & TypeScript")" ]; then
        sudo dnf -y install nodejs npm
        sudo npm install -g typescript
        [ "$(y_n "Vue.js?")" ] && sudo npm install -g @vue/cli
        [ "$(y_n "Angular?")" ] && sudo npm install -g @angular/cli
    fi

    # .NET Core SDK
    if [ "$(y_n ".NET Core")" ]; then
        sudo dnf copr enable @dotnet-sig/dotnet
        sudo dnf -y install dotnet-sdk-2.1
    fi

    # Python
    if [ "$(y_n "Python")" ]; then
        sudo dnf -y install python python3 python2-devel python3-devel \
            python2-pip #python3-pip
    fi

    # Haskell
    [ "$(y_n "Haskell")" ] && sudo dnf -y install ghc-compiler

    # Clojure
    if [ "$(y_n "Clojure")" ]; then
        curl -O https://download.clojure.org/install/linux-install-1.9.0.381.sh
        chmod +x linux-install-1.9.0.381.sh
        sudo ./linux-install-1.9.0.381.sh
        rm linux-install-1.9.0.381.sh
    fi

    # Go
    [ "$(y_n "Go")" ] && sudo dnf -y install go

    # C
    if [ "$(y_n "C")" ]; then
        sudo dnf -y install make cmake autoconf automake gcc
        sudo dnf -y groupinstall "C Development Tools and Libraries"
    fi

    # TODO C++
    if [ "$(y_n "C++")" ]; then
        # TODO g++ clang-tidy
        sudo dnf -y install gcc-c++ clang gtkmm30-devel clang-tools-extra cppcheck

        # Bitcoin Core
        sudo dnf install libtool openssl-devel \
            libevent-devel boost-devel libdb4-devel libdb4-cxx-devel
        # Bitcoin Qt
        sudo dnf install miniupnpc-devel qt5-qttools-devel qt5-qtbase-devel \
            protobuf-devel qrencode-devel
    fi

    # TODO Other tools
    # docker gtk+ qt (split from Bitcoin) libvtemm-devel
    #
    # Plan 9 User Space & Suckless tools
    # sudo dnf -y install libXt-devel

fi


echo "========================================"
echo "-------- System Configuration ----------"
echo "========================================"

# TODO install dotfiles?
# TODO switch GNOME to use default themes
# TODO use dark theme, set caps to ctrl
# TODO install fonts (use custom font installer)

# TODO Create basic directory structure
# mkdir -p ~/Documents/{Projects,Notes}

# Generate SSH keys
[ "$(y_n "Generate SSH keys?")" ] && ssh-keygen -t rsa -b 4096


# TODO install applications from source
