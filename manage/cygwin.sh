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

# Set up mintty
if [ "$(y_n "Configure MinTTY?")" ]; then
    cat << EOF > .minttyrc
BoldAsFont=-1
CursorType=block
Font=Consolas # Inconsolata
FontHeight=14
FontSmoothing=full
Locale=en_GB
Charset=UTF-8
Scrollbar=none
Term=xterm-256color
BellType=0
AllowBlinking=no
CursorBlinks=no
Transparency=off
CtrlShiftShortcuts=no
ComposeKey=off
BoldAsColour=yes

#ThemeFile=flat-ui
BackgroundColour=38,38,38
ForegroundColour=238,238,238
CursorColour=238,238,238
Black=38,38,38
BoldBlack=85,87,83
Red=207,63,97
BoldRed=239,41,41
Green=123,183,91
BoldGreen=138,226,52
Yellow=233,179,42
BoldYellow=252,233,79
Blue=52,101,164
BoldBlue=114,159,207
Magenta=165,127,196
BoldMagenta=173,127,168
Cyan=56,154,173
BoldCyan=52,226,226
White=250,250,246
BoldWhite=238,238,238
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
    rm ~/.profile ~/.bashrc ~/.inputrc
    git clone https://github.com/axvr/dotfiles.git ~/dotfiles
    cd ~/dotfiles/
    stow -t ~ bin/ shell/ git/ tmux/ vim/
    cd "$HOME"

    # Improve dotfiles for Cygwin
    cat << EOF >> ~/dotfiles/shell/.bashrc

if [ "\$(uname -o)" == "Cygwin" ]; then
    alias dotnet="winpty dotnet"
    alias node="winpty node"
    alias npm="winpty npm.cmd"
    alias ng="winpty ng.cmd"
    alias choco="winpty choco"
fi
EOF
    read -rp "Email address: " email
    sed -i "s/\(email = \).*$/\1$email/" ~/dotfiles/git/.config/git/config
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
