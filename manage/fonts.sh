#!/bin/sh

# Install fonts

font_dir="$HOME/.fonts"
mkdir -p "$font_dir"

# TODO create installers for other fonts
font_list=$(cat <<EOF
Tamsyn          (http://www.fial.com/~scott/tamsyn-font/)
Iosevka         (https://be5invis.github.io/Iosevka/)
Roboto          (https://fonts.google.com/specimen/Roboto)
Roboto Mono
Roboto Slab
Source Code Pro (https://adobe-fonts.github.io/source-code-pro/)
Monospace
Liberation Mono
Hack
Noto
Terminus
Fira
Fira Code
EOF
)

tamsyn() {
    printf "Installing font: Tamsyn\\n"
    rm -r "$font_dir/tamsyn-font-1.11" "$font_dir/tamsyn-font-1.11.tar.gz"
    tamsyn="http://www.fial.com/~scott/tamsyn-font/download/tamsyn-font-1.11.tar.gz"
    curl -#L "$tamsyn" -o "$font_dir/tamsyn-font-1.11.tar.gz"
    tar -zxf "$font_dir/tamsyn-font-1.11.tar.gz" -C "$font_dir/"
}

[ ! -n "$1" ] && printf "$font_list\\n"
for f in "$@"; do "${f,,}"; done
