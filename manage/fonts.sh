#!/bin/sh

# A simple font installer for Unix-like OSs
#
# Author:  Alex Vear (axvr)
# Licence: Unlicence

font_dir="$HOME/.fonts"
mkdir -p "$font_dir"

font_list=$(cat <<EOF
[x] Tamsyn              (http://www.fial.com/~scott/tamsyn-font/)
[ ] Iosevka             (https://be5invis.github.io/Iosevka/)
[x] Roboto              (https://fonts.google.com/specimen/Roboto)
[x] Roboto Mono         (https://fonts.google.com/specimen/Roboto+Mono)
[x] Roboto Slab         (https://fonts.google.com/specimen/Roboto+Slab)
[x] Roboto Condensed    (https://fonts.google.com/specimen/Roboto+Condensed)
[x] Source Code Pro     (https://adobe-fonts.github.io/source-code-pro/)
[ ] DejaVu Sans         (https://dejavu-fonts.github.io/)
[ ] DejaVu Serif        (https://dejavu-fonts.github.io/)
[ ] DejaVu Sans Mono    (https://dejavu-fonts.github.io/)
[ ] Liberation Sans
[ ] Liberation Mono
[ ] Hack                (https://sourcefoundry.org/hack/)
[x] Noto Sans           (https://fonts.google.com/specimen/Noto+Sans)
[x] Noto Serif          (https://fonts.google.com/specimen/Noto+Serif)
[ ] Terminus            (http://terminus-font.sourceforge.net/)
[x] Inconsolata         (http://www.levien.com/type/myfonts/inconsolata.html)
[ ] Mononoki            (https://madmalik.github.io/mononoki/)
[x] Fira Sans           (https://mozilla.github.io/Fira/)
[x] Fira Mono           (https://mozilla.github.io/Fira/)
[ ] Fira Code           (https://github.com/tonsky/FiraCode)
[ ] Font Awesome        (https://fontawesome.com/)
[ ] Anonymous Pro       (https://www.marksimonson.com/fonts/view/anonymous-pro)
EOF
)

[ ! $(command -v curl)] && printf "Error: cURL is not installed\\n"

# XXX Capitalise the first letter of each word in a string (fully POSIX)
capitalise() {
    for i in $(printf "$1" | tr ' ' '_' | sed -e 's/^/_&/' -e 's/[^_]/& /g')
    do
        if [ "$(printf "$i" | grep "^_")" ]; then
            printf "$i" | tr '[:lower:]' '[:upper:]'
        else
            printf "$i"
        fi | sed 's/^_/ /'
    done | sed -e 's/^ *//' -e 's/ *$//'
}

# Simplify installiing fonts from Google Fonts
google_fonts() {
    name="$(capitalise "$1" | sed -e 'y/-_/  /')"
    url="https://fonts.google.com/download?family=$(printf "$name" | sed 's/\s/%20/g')"
    printf "Installing font: %s\\n" "$name"
    rm -r "${font_dir:?}/${1}/"
    curl -L "$url" -o "$font_dir/$1/$1.zip" --create-dirs
    unzip "$font_dir/$1/$1.zip" -d "$font_dir/$1/"
}

general_tar_gz() {
    true
}

general_zip() {
    true
}

tamsyn() {
    printf "Installing font: Tamsyn\\n"
    rm -r "$font_dir/tamsyn-font-1.11/"
    tamsyn="http://www.fial.com/~scott/tamsyn-font/download/tamsyn-font-1.11.tar.gz"
    curl -L "$tamsyn" -o "$font_dir/tamsyn-font-1.11/tamsyn.tar.gz" --create-dirs
    tar -zxvf "$font_dir/tamsyn-font-1.11/tamsyn.tar.gz" -C "$font_dir/"
}

roboto() {
    google_fonts "$f"
}

roboto_mono() {
    google_fonts "$f"
}

roboto_slab() {
    google_fonts "$f"
}

roboto_condensed() {
    google_fonts "$f"
}

source_code_pro() {
    google_fonts "$f"
}

noto_sans() {
    google_fonts "$f"
}

noto_serif() {
    google_fonts "$f"
}

inconsolata() {
    google_fonts "$f"
}

fira_sans() {
    google_fonts "$f"
}

fira_mono() {
    google_fonts "$f"
}

[ ! -n "$1" ] && printf "%s\\n" "$font_list"
for f in "$@"; do "${f,,}"; done
