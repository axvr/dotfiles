#!/usr/bin/env bash

# Set up minimal Cygwin install

# useful to know: "rundll32 sysdm.cpl,EditEnvironmentVariables"

y_n() {
    unset check
    read -rp "$1 [y/N]: " check
    case "$check" in
        [yY])   echo " ";;
        *)      return 0;;
    esac
}

cat << EOF 
Install these tools first:
* vim
* tmux
* ctags
* stow
* git
* perl
* python2
* python3
* bash
* curl
* bash-completion
* unzip
* wget
* less
* make
* gcc-core
* gcc-g++
* mingw64-x86_64-gcc-g++ or mingw64-i386-gcc-g++
* openssh
* tree
* emacs
* emacs-w32
EOF

[ ! "$(y_n "Have you installed the above tools?")" ] && exit 0


# Set up mintty
if [ "$(y_n "Configure MinTTY?")" ]; then
    cat << EOF > .minttyrc
BoldAsFont=-1
ThemeFile=flat-ui
CursorType=block
Font=Consolas
FontHeight=11
Locale=en_GB
Charset=UTF-8
Scrollbar=none
Term=xterm-256color
BellType=0
AllowBlinking=yes
CursorBlinks=no
Transparency=off
BackgroundColour=17,16,20
CtrlShiftShortcuts=no
ComposeKey=off
BoldAsColour=yes
EOF
fi

# Compile and install winpty
if [ "$(y_n "Install winpty?")" ]; then
    git clone https://github.com/rprichard/winpty.git ~/winpty
    cd ~/winpty/
    ./configure
    make
    make install
    cd "$HOME"
fi

# Get dotfiles and stow them
if [ "$(y_n "Install dotfiles?")" ]; then
    rm ~/.bash_profile ~/.bashrc ~/.inputrc
    git clone https://github.com/axvr/dotfiles.git ~/dotfiles
    cd ~/dotfiles/
    stow -t ~ bin/ bash/ git/ tmux/ vim/
    cd "$HOME"

    # Improve dotfiles for Cygwin
    cat << EOF >> ~/dotfiles/bash/.bashrc

if [ "\$(uname -o)" == "Cygwin" ]; then
    alias ls="ls --color=auto"
    alias grep="grep --color=auto"
    alias egrep="egrep --color=auto"
    alias fgrep="fgrep --color=auto"
    alias pgrep="pgrep --color=auto"

    alias dotnet="winpty dotnet"
    alias node="winpty node"
    alias npm="winpty npm.cmd"
    alias ng="winpty ng.cmd"
    alias choco="winpty choco"
fi
EOF
    read -rp "Email address: " email
    sed -i "s/\(email = \).*$/\1$email/" ~/dotfiles/git/.gitconfig
fi

# Create symbolic links for ease of use
if [ "$(y_n "Create symbolic links?")" ]; then
    ln -s "/cygdrive/c/Users/$USER/" ~/local
    mkdir -p "/cygdrive/c/Users/$USER/source/repos/"
    ln -s "/cygdrive/c/Users/$USER/source/repos/" ~/code
    read -rp "Remote location: " remote
    remote="$(echo "$remote" | sed 's/\\/\//g')"
    ln -s "$remote" ~/remote
fi

# Set up SSH
[ "$(y_n "Create SSH keys?")" ] && ssh-keygen -t rsa -b 4096
