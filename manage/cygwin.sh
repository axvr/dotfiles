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
    stow -t ~ bin/ shell/ git/ tmux/ vim/ cygwin/
    cd "$HOME"

    # Change global Git email address
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
