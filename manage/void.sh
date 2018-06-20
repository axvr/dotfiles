#!/bin/sh

sudo xbps-install -Suv

sudo xbps-install -S patch tmux ctags vim make stow git curl
sudo xbps-install -S xorg-minimal xorg-fonts xf86-video-intel
sudo xbps-install -S libX11 libX11-devel libXft libXft-devel pkg-config \
    libXinerama libXinerama-devel libXrandr libXrandr-devel
sudo xbps-install -S alsa-utils abcde cdparanoia cmus
sudo xbps-install -S scrot dunst newsboat ledger weechat ffmpeg flac unzip wget
sudo xbps-install -S firefox

# TODO install fontconfig & dbus

# TODO set up LFVS https://fwupd.org/
sudo xbps-install -S fwupd fwupdate

mkdir -p "$HOME/docs" "$HOME/code/suckless"
git clone https://github.com/axvr/dotfiles "$HOME/code/dotfiles"
git clone https://git.suckless.org/dwm   "$HOME/code/suckless/dwm"
git clone https://git.suckless.org/st    "$HOME/code/suckless/st"
git clone https://git.suckless.org/dmenu "$HOME/code/suckless/dmenu"
git clone https://git.suckless.org/slock "$HOME/code/suckless/slock"
# TODO install plan 9 tools
# TODO run font installer (install tamsyn)
$HOME/code/dotfiles/manage/fonts.sh tamsyn
# TODO auto-compile and install suckless tools (and patch them)

sudo printf "%wheel ALL=(ALL) NOPASSWD: /home/axvr/bin/bright\\n" >> /etc/sudoers
sudo printf "%wheel ALL=(ALL) NOPASSWD: /usr/bin/halt, /usr/bin/poweroff, \
	/usr/bin/reboot, /usr/bin/shutdown, /usr/bin/zzz, /usr/bin/ZZZ\\n" >> /etc/sudoers
