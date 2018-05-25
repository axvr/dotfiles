#!/bin/sh
# OmniSharp-roslyn Management tool

usage() {
    printf "usage: %s [-i] [-r] [-v version] [-V] [-l location] [-H] [-u]\\n" "$0"
}

# From: https://gist.github.com/lukechilds/a83e1d7127b78fef38c2914c4ececc3c
get_latest_version() {
    curl --silent "https://api.github.com/repos/OmniSharp/omnisharp-roslyn/releases/latest" |
    grep '"tag_name":' | sed -E 's/.*"([^"]+)".*/\1/'
}
get_current_version() {
    # TODO implement this
    printf "???"
}


# Options:
# * i :: install (will also update if already installed)
# * r :: remove
# * v :: version to use (otherwise use latest)
# * V :: print the version information
# * l :: where to install the server
# * u :: help / usage info
# * H :: install the HTTP version of the server
# Options to add later:
# * M :: type of server: mono or regular
# * S :: start the server (takes a solution file)

LOCATION="$HOME/.omnisharp/"

while getopts irv:Vl:uH o "$@"
do
    case "$o" in
        i)      install;;
        r)      remove;;
        v)      VERSION="$OPTARG";;
        V)
            printf "Latest version:    %s\\n" "$(get_latest_version)"
            printf "Installed version: %s\\n" "$(get_current_version)"
            exit 0
            ;;
        l)      LOCATION="$OPTARG";;
        H)      HTTP=".http";;
        u)      usage && exit 0;;
        [?])    usage && exit 1;;
    esac
done

case "$(uname -s)" in
    "Linux")    OS="linux";;
    "Darwin")   OS="osx";;
    *)          exit 1;;
esac
case "$(uname -m)" in
    "x86_64")   MACHINE="x64";;
    "i368")     MACHINE="x86";;
    *)          exit 1;;
esac
FILE_NAME=$(printf "omnisharp%s-%s-%s.tar.gz" "$HTTP" "$OS" "$MACHINE")

if [ -z "$VERSION" ]; then
    VERSION="$(get_latest_version)"
fi
BASE_URL="https://github.com/OmniSharp/omnisharp-roslyn/releases/download"
FULL_URL="${BASE_URL}/${VERSION}/${FILE_NAME}"
echo "$FULL_URL"
