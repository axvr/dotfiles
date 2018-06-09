#!/bin/sh

# A simple font installer for Unix-like OSs
#
# Author:  Alex Vear (axvr)
# Licence: Unlicence

[ ! $(command -v curl) ] && printf "Error: cURL is not installed\\n"

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

# Simplify installing fonts from Google Fonts
google_fonts() {
    url="https://fonts.google.com/download?family=$(printf "$1" | sed 's/\s/%20/g')"
    printf "Installing font: %s\\n" "$1"
    rm -r "${font_dir:?}/${f}/"
    curl -L "$url" -o "$font_dir/$f/$f.zip" --create-dirs
    unzip "$font_dir/$f/$f.zip" -d "$font_dir/$f/"
}

[ ! -n "$1" ] && printf "%s\\n" "$font_list"
for f in "$@"
do 
    f="$(printf "${f,,}" | tr -s ' _-' '_')"
    case "$f" in
        roboto)             google_fonts "Roboto";;
        roboto_mono)        google_fonts "Roboto Mono";;
        roboto_slab)        google_fonts "Roboto Slab";;
        roboto_condensed)   google_fonts "Roboto Condensed";;
        source_code_pro)    google_fonts "Source Code Pro";;
        noto_sans)          google_fonts "Noto Sans";;
        noto_serif)         google_fonts "Noto Serif";;
        inconsolata)        google_fonts "Inconsolata";;
        fira_sans)          google_fonts "Fira Sans";;
        fira_mono)          google_fonts "Fira Mono";;
        tamsyn)
            printf "Installing font: Tamsyn\\n"
            rm -r "$font_dir/tamsyn-font-1.11/"
            url="http://www.fial.com/~scott/tamsyn-font/download/tamsyn-font-1.11.tar.gz"
            curl -L "$url" -o "$font_dir/tamsyn-font-1.11/tamsyn.tar.gz" --create-dirs
            tar -zxvf "$font_dir/tamsyn-font-1.11/tamsyn.tar.gz" -C "$font_dir/"
            ;;
        *)                  printf "Error: Invalid font '%s'\\n" "$f"
    esac
done
