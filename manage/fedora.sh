#!/usr/bin/env bash
# -----------------------------
# Set up Fedora
# -----------------------------

# Run install script using this command
# wget https://raw.githubusercontent.com/axvr/dotfiles/master/manage/fedora.sh && bash fedora.sh

# Fedora Broadcom WiFi drivers
# https://ashhar24.wordpress.com/2012/06/15/setting-up-wireless-driver-fedora/


# -------------------------------------------------------------------------------

# Upgrade Fedora system
function upgrade_system() {
    sudo dnf -y upgrade
}

# Function to install applications for Fedora (first install)
function install_applications() {

    # Install Applications
    sudo dnf -y install keepassxc polari gnome-tweak-tool asunder \
         gnome-todo libreoffice torbrowser-launcher tilix inkscape

    # Install Development Tools (Some may need to be installed via pip)
    sudo dnf -y install emacs vim neovim \
         texlive-scheme-basic texlive-titling texlive-titlesec \
         texlive-roboto texlive-noto \
         python python3 pylint python3-pylint python-nose python3-nose \
         python2-devel python3-devel python2-flake8 python3-flake8 \
         rust cargo \
         cppcheck clang gtkmm30-devel \
         cmake ctags ShellCheck perl
    cargo install rustfmt
    cargo install racer # TODO set up rust racer

    sudo dnf -y groupinstall "Development Tools" \
         "C Development Tools and Libraries" \
         "GNOME Software Development"

}


# Install Font Packs
function install_fonts() {
    mkdir -p ~/.fonts
    sudo dnf -y install adobe-source-code-pro-fonts \
         google-roboto-fonts google-roboto-mono-fonts \
         google-noto-fonts-common google-noto-mono-fonts
    # TODO install Iosevka font in `~/.fonts/Iosevka/` or compile and render
}


# Install GTK+ & Qt Themes, Wallpapers & Cursor Themes
function install_themes() {
    mkdir -p ~/.themes
    sudo dnf -y install arc-theme breeze-cursor-theme gnome-backgrounds-extras \
         sassc inkscape
    # Adapta theme
    sudo rm -rf /usr/share/themes/{Adapta,Adapta-Eta,Adapta-Nokto,Adapta-Nokto-Eta}
    rm -rf ~/.local/share/themes/{Adapta,Adapta-Eta,Adapta-Nokto,Adapta-Nokto-Eta}
    rm -rf ~/.themes/{Adapta,Adapta-Eta,Adapta-Nokto,Adapta-Nokto-Eta}
    rm -rf /tmp/adapta
    git clone https://github.com/adapta-project/adapta-gtk-theme.git /tmp/adapta
    current_location=$(pwd)
    cd /tmp/adapta/ || exit
    ./autogen.sh
    make && sudo make install
    cd "$current_location" || exit
}


# Install Icon Packs
function install_icons() {
    rm -rf /tmp/arc-temp
    rm -r ~/.icons/Arc
    mkdir -p ~/.icons/Arc
    git clone https://github.com/horst3180/arc-icon-theme.git /tmp/arc-temp
    mv /tmp/arc-temp/Arc/* ~/.icons/Arc/
}


# Install GNOME Extensions
function install_extensions() {
    rm -rf /tmp/dpt
    mkdir -p ~/.local/share/gnome-shell/extensions
    rm -r ~/.local/share/gnome-shell/extensions/dynamic-panel-transparency@rockon999.github.io
    git clone https://github.com/rockon999/dynamic-panel-transparency.git \
        /tmp/dpt
    mv /tmp/dpt/dynamic-panel-transparency@rockon999.github.io \
       ~/.local/share/gnome-shell/extensions/
    # TODO add these extensions
    # * Hide Top Bar
    # * Focusli (if fixed)
    # * Dash to Dock
    # * User Themes (check if pre installed with GNOME)
    # * Places Status Indicator (check if pre installed with GNOME)
    # * Background Logo (pre-installed on Fedora)
    # * AlternateTab (check if pre installed with GNOME)
    # * Workspaces to Dock
}


# Set up applications and load dot-files
function setup_applications() {

    # Set up Development Tools
    rm -rf ~/.emacs.d/
    mkdir -p ~/.emacs.d ~/.config/nvim
    github=https://raw.githubusercontent.com/axvr/dotfiles/master
    wget $github/spacemacs/spacemacs -O ~/.spacemacs
    wget $github/neovim/init.vim     -O ~/.config/nvim/init.vim
    wget $github/vim/vimrc           -O ~/.vimrc
    wget $github/git/gitconfig       -O ~/.gitconfig
    wget $github/bash/bashrc         -O ~/.bashrc
    source ~/.bashrc
    git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d

    # Install gnome-shell-ext-conf (https://github.com/cyberalex4life/gnome-shell-extension-cl)
    sudo wget https://raw.githubusercontent.com/cyberalex4life/gnome-shell-extension-cl/master/gnome-shell-extension-cl \
         -O /usr/local/bin/gnome-shell-ext-conf
    sudo chmod +x /usr/local/bin/gnome-shell-ext-conf

}


# TODO maybe add ssh key gen (ssh-keygen -t rsa -b 4096 -C "email@here.com")


# Add dotfiles to the dotfiles repo
function update_repo() {

    # TODO mkdir -p ...

    # Vim - vimrc
    cp ~/.vimrc ~/Documents/Projects/dotfiles/vim/vimrc

    # Neovim - init.vim
    cp ~/.config/nvim/init.vim ~/Documents/Projects/dotfiles/neovim/init.vim

    # Spacemacs - spacemacs
    cp ~/.spacemacs ~/Documents/Projects/dotfiles/spacemacs/spacemacs

    # Clang Format - clang-format
    #cp ~/Documents/Projects/mozilla/firefox/.clang-format ~/Documents/Projects/dotfiles/clang/clang-format
    #cp ~/Documents/Projects/mozilla/firefox/.clang-format-ignore ~/Documents/Projects/dotfiles/clang/clang-format-ignore
    #cp ~/Documents/Projects/mozilla/firefox/.clang-tidy ~/Documents/Projects/dotfiles/clang/clang-tidy

    # Bash - bashrc
    cp ~/.bashrc ~/Documents/Projects/dotfiles/bash/bashrc

    # Git - gitconfig
    cp ~/.gitconfig ~/Documents/Projects/dotfiles/git/gitconfig

    # Xresources - Xresources
    #cp ~/.Xresources ~/Documents/Projects/dotfiles/customise/urxvt/Xresources

    # Sway - config
    #cp ~/.config/sway/config ~/Documents/Projects/dotfiles/customise/sway/config

    # Qutebrowser
    #cp ~/.config/qutebrowser/* ~/Documents/Projects/dotfiles/customise/qutebrowser/

}


# -------------------------------------------------------------------------------

# Complete theme selection

function default_theme() {

    # Extensions configuration
    gnome-shell-ext-conf -e 'user-theme@gnome-shell-extensions.gcampax.github.com'
    gnome-shell-ext-conf -d 'dynamic-panel-transparency@rockon999.github.io'

    # Theme configuration
    gsettings set org.gnome.desktop.interface cursor-theme   'Adwaita'
    gsettings set org.gnome.desktop.interface icon-theme     'Adwaita'
    gsettings set org.gnome.shell.extensions.user-theme name 'Adwaita'
    gsettings set org.gnome.desktop.interface gtk-theme      'Adwaita'

}


function adapta-eta_theme() {

    # Extensions configuration
    gnome-shell-ext-conf -e 'user-theme@gnome-shell-extensions.gcampax.github.com'
    gnome-shell-ext-conf -e 'dynamic-panel-transparency@rockon999.github.io'

    # Theme configuration
    gsettings set org.gnome.desktop.interface cursor-theme   'Breeze_Snow'
    gsettings set org.gnome.desktop.interface icon-theme     'Arc'
    gsettings set org.gnome.shell.extensions.user-theme name 'Adapta-Nokto-Eta'
    gsettings set org.gnome.desktop.interface gtk-theme      'Adapta-Eta'

}


function adapta_theme() {

    # Extensions configuration
    gnome-shell-ext-conf -e 'user-theme@gnome-shell-extensions.gcampax.github.com'
    gnome-shell-ext-conf -e 'dynamic-panel-transparency@rockon999.github.io'

    # Theme configuration
    gsettings set org.gnome.desktop.interface cursor-theme   'Breeze_Snow'
    gsettings set org.gnome.desktop.interface icon-theme     'Arc'
    gsettings set org.gnome.shell.extensions.user-theme name 'Adapta-Nokto'
    gsettings set org.gnome.desktop.interface gtk-theme      'Adapta'

}


function arc_theme() {

    # Extensions configuration
    gnome-shell-ext-conf -e 'user-theme@gnome-shell-extensions.gcampax.github.com'
    gnome-shell-ext-conf -e 'dynamic-panel-transparency@rockon999.github.io'

    # Theme configuration
    gsettings set org.gnome.desktop.interface cursor-theme   'Breeze_Snow'
    gsettings set org.gnome.desktop.interface icon-theme     'Arc'
    gsettings set org.gnome.shell.extensions.user-theme name 'Arc-Dark'
    gsettings set org.gnome.desktop.interface gtk-theme      'Arc-Darker'

}


# -------------------------------------------------------------------------------

function user_selection() {

    # User selection
    printf "
Select an option from below:
[1] : Set up entire Fedora install,
[2] : Install applications
[3] : Upgrade fonts, themes, icons and extensions,
[4] : Set up applications (install dotfiles),
[5] : Update dotfiles repository,
[6] : Load a GNOME theme,
[0] : Cancel / Exit.

Selection: "

    read -r selection
    printf "\n"

    if [ "$selection" = "1" ]
    then
        upgrade_system
        install_applications
        install_fonts
        install_themes
        install_icons
        install_extensions
        setup_applications
        exitValue=1
        message="Fedora system was set up"

    elif [ "$selection" = "2" ]
    then
        upgrade_system
        install_applications
        message="Applications were installed"

    elif [ "$selection" = "3" ]
    then
        install_fonts
        install_themes
        install_icons
        install_extensions
        message="Customisations were installed"

    elif [ "$selection" = "4" ]
    then
        setup_applictions
        message="Applications configured and set up"

    elif [ "$selection" = "5" ]
    then
        update_repo
        message="Dotfiles repository was updated"

    elif [ "$selection" = "6" ]
    then
        1;

    elif [ "$selection" = "0" ]
    then
        # Exit set up script
        exitValue=1

    else
        # Error message
        message="ERROR: \"$selection\" is not a valid option"
    fi

}


# -------------------------------------------------------------------------------

exitValue=0
message="Fedora Configuration Script"

while [ "$exitValue" != "1" ]
do
    printf "\n%s\n" "$message"
    user_selection
done

exit

