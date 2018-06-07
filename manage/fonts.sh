#!/bin/sh

# Install fonts

font_dir="$HOME/.fonts"
mkdir -p "$font_dir"

font_list=$(cat <<EOF
Tamsyn  (http://www.fial.com/~scott/tamsyn-font/)
Iosevka (https://be5invis.github.io/Iosevka/)
Roboto  (https://fonts.google.com/specimen/Roboto)
EOF
)

# TODO add these fonts:
# * Iosevka
# * Roboto
# * Roboto Mono
# * Roboto Slab
# * Fira
# * Fira Code
# * Liberation Mono
# * Monospace
# * and more

tamsyn() {
    printf "Installing font: Tamsyn\\n"
    rm -r "$font_dir/tamsyn-font-1.11" "$font_dir/tamsyn-font-1.11.tar.gz"
    tamsyn="http://www.fial.com/~scott/tamsyn-font/download/tamsyn-font-1.11.tar.gz"
    curl -#L "$tamsyn" -o "$font_dir/tamsyn-font-1.11.tar.gz"
    tar -zxf "$font_dir/tamsyn-font-1.11.tar.gz" -C "$font_dir/"
}

[ ! -n "$1" ] && printf "$font_list\\n" && exit
for f in "$@"; do "${f,,}"; done
