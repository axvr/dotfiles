#!/bin/sh

# A simple font installer for Unix-like OSs
#
# Author:  Alex Vear (axvr)
# Licence: Unlicence

# TODO: Simplify this, and attempt to reduce code duplication
# TODO: Provide a mechanism to install the fonts to system directories

requires() {
    for i in "$@"
    do
        if [ ! "$(command -v "$i")" ]
        then
            (>&2 printf "Error: '%s' is not installed\\n" "$i")
            exit
        fi
    done
}
requires "curl"

github_latest_version() {
    curl --silent "https://api.github.com/repos/$1/$2/releases/latest" |
    grep '"tag_name":' | sed -E 's/.*"([^"]+)".*/\1/'
}

font_dir="$HOME/.fonts"
mkdir -p "$font_dir"

# TODO: Input (http://input.fontbureau.com/) - Ask for permission to distribute
# TODO: Iosevka (https://be5invis.github.io/Iosevka/)
# TODO: Liberation Mono (find home page)
# TODO: Liberation Sans (find home page)
# TODO: Terminus (http://terminus-font.sourceforge.net/)

font_list=$(cat <<EOF
Anonymous Pro       (https://www.marksimonson.com/fonts/view/anonymous-pro)
DejaVu Sans/Serif   (https://dejavu-fonts.github.io/)
Dijkstra            (http://lucacardelli.name/indexartifacts.html)
Fira Code           (https://github.com/tonsky/FiraCode)
Fira Mono           (https://mozilla.github.io/Fira/)
Fira Sans           (https://mozilla.github.io/Fira/)
Font Awesome        (https://fontawesome.com/)
Hack                (https://sourcefoundry.org/hack/)
Inconsolata         (http://www.levien.com/type/myfonts/inconsolata.html)
Mononoki            (https://madmalik.github.io/mononoki/)
Noto Sans           (https://fonts.google.com/specimen/Noto+Sans)
Noto Serif          (https://fonts.google.com/specimen/Noto+Serif)
Roboto Condensed    (https://fonts.google.com/specimen/Roboto+Condensed)
Roboto              (https://fonts.google.com/specimen/Roboto)
Roboto Mono         (https://fonts.google.com/specimen/Roboto+Mono)
Roboto Slab         (https://fonts.google.com/specimen/Roboto+Slab)
Share Tech Mono     (https://fonts.google.com/specimen/Share+Tech+Mono)
Source Code Pro     (https://adobe-fonts.github.io/source-code-pro/)
Tamsyn              (http://www.fial.com/~scott/tamsyn-font/)
EOF
)

# Simplify installing fonts from Google Fonts
google_fonts() {
    requires "unzip"
    url="https://fonts.google.com/download?family=$(printf "$1" | sed 's/\s/%20/g')"
    printf "Installing font: %s\\n" "$1"
    [ -d "${font_dir:?}/${f}/" ] && rm -r "${font_dir:?}/${f}/"
    curl -L "$url" -o "$font_dir/$f/$f.zip" --create-dirs
    unzip "$font_dir/$f/$f.zip" -d "$font_dir/$f/"
}

[ ! -n "$1" ] && printf "%s\\n" "$font_list"
for f in "$@"
do 
    f="$(printf "$f" | tr '[:upper:]' '[:lower:]' | tr -s ' _-' '_')"
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
        share_tech_mono)    google_fonts "Share Tech Mono";;

        fira_code)
            requires "unzip"
            printf "Installing font: Fira Code\\n"
            [ -d "$font_dir/fira-code/" ] && rm -r "$font_dir/fira-code/"
            version="$(github_latest_version "tonsky" "FiraCode")"
            url="https://github.com/tonsky/FiraCode/releases/download/$version/FiraCode_$version.zip"
            curl -L "$url" -o "$font_dir/fira-code/FiraCode_$version.zip" --create-dirs
            unzip "$font_dir/fira-code/FiraCode_$version.zip" -d "$font_dir/fira-code/"
            ;;

        hack)
            requires "gzip"
            printf "Installing font: Hack\\n"
            [ -d "$font_dir/hack/" ] && rm -r "$font_dir/hack/"
            version="$(github_latest_version "source-foundry" "Hack")"
            url="https://github.com/source-foundry/Hack/releases/download/$version/Hack-$version-ttf.tar.gz"
            curl -L "$url" -o "$font_dir/hack/Hack-$version-ttf.tar.gz" --create-dirs
            tar -zxvf "$font_dir/hack/Hack-$version-ttf.tar.gz" -C "$font_dir/hack"
            ;;

        mononoki)
            requires "unzip"
            printf "Installing font: Mononoki\\n"
            [ -d "$font_dir/mononoki/" ] && rm -r "$font_dir/mononoki/"
            version="$(github_latest_version "madmalik" "mononoki")"
            url="https://github.com/madmalik/mononoki/releases/download/$version/mononoki.zip"
            curl -L "$url" -o "$font_dir/mononoki/mononoki.zip" --create-dirs
            unzip "$font_dir/mononoki/mononoki.zip" -d "$font_dir/mononoki/"
            ;;

        dejavu)
            requires "bzip2"
            printf "Installing font: DejaVu\\n"
            [ -d "$font_dir/dejavu-fonts-ttf-2.37/" ] && rm -r "$font_dir/dejavu-fonts-ttf-2.37/"
            url="http://sourceforge.net/projects/dejavu/files/dejavu/2.37/dejavu-fonts-ttf-2.37.tar.bz2"
            curl -L "$url" -o "$font_dir/dejavu-fonts-ttf-2.37/dejavu-fonts-ttf-2.37.tar.bz2" --create-dirs
            tar -jxvf "$font_dir/dejavu-fonts-ttf-2.37/dejavu-fonts-ttf-2.37.tar.bz2" -C "$font_dir/"
            ;;

        tamsyn)
            requires "gzip"
            printf "Installing font: Tamsyn\\n"
            [ -d "$font_dir/tamsyn-font-1.11/" ] && rm -r "$font_dir/tamsyn-font-1.11/"
            url="http://www.fial.com/~scott/tamsyn-font/download/tamsyn-font-1.11.tar.gz"
            curl -L "$url" -o "$font_dir/tamsyn-font-1.11/tamsyn.tar.gz" --create-dirs
            tar -zxvf "$font_dir/tamsyn-font-1.11/tamsyn.tar.gz" -C "$font_dir/"
            ;;

        anonymous_pro)
            requires "unzip"
            printf "Installing font: Anonymous Pro\\n"
            [ -d "$font_dir/anonymous-pro/" ] && rm -r "$font_dir/anonymous-pro/"
            archive_name="AnonymousPro-1.002.zip"
            url="https://www.marksimonson.com/assets/content/fonts/$archive_name"
            curl -L "$url" -o "$font_dir/anonymous-pro/$archive_name" --create-dirs
            unzip "$font_dir/anonymous-pro/$archive_name" -d "$font_dir/anonymous-pro"
            ;;

        font_awesome)
            requires "unzip"
            printf "Installing font: Font Awesome (free)\\n"
            [ -d "$font_dir/font-awesome/" ] && rm -r "$font_dir/font-awesome/"
            archive_name="fontawesome-free-5.2.0-desktop.zip"
            url="https://use.fontawesome.com/releases/v5.2.0/$archive_name"
            curl -L "$url" -o "$font_dir/font-awesome/fontawesome-free-5.2.0-desktop.zip" --create-dirs
            unzip "$font_dir/font-awesome/$archive_name" -d "$font_dir/font-awesome/"
            mv "$font_dir/font-awesome/fontawesome-free-5.2.0-desktop/"* "$font_dir/font-awesome/"
            rmdir "$font_dir/font-awesome/fontawesome-free-5.2.0-desktop/"
            ;;

        dijkstra)
            printf "Installing font: Dijkstra\\n"
            [ -d "$font_dir/dijkstra/" ] && rm -r "$font_dir/dijkstra/"
            url="http://lucacardelli.name/Artifacts/Fonts/Pc/dijkstra.ttf"
            curl -L "$url" -o "$font_dir/dijkstra/dijkstra.ttf" --create-dirs
            ;;

        *)  printf "Error: Invalid font '%s'\\n" "$f";;
    esac
done
