#!/usr/bin/env bash
# -------------------------
# Manage Dotfiles
# -------------------------

# Install dots on your system:
# $ wget https://raw.githubusercontent.com/axvr/dotfiles/master/manage/dots.sh -O /usr/local/bin/dots
# $ chmod +x /usr/local/bin/dots
# Or just execute the downloaded dots.sh file


# Set location to store dotfiles
dotfile_local_location="Documents/Projects/dotfiles"
dotfile_location="$HOME/$dotfile_local_location"
# File name to run
dotdotdot_file=".dot.sh"
# Ensure the directory exists
mkdir -p "$dotfile_location"


function install_dotfiles() {

    # TODO run all
    arguments=("$@")
    unset "arguments[0]"

    for selected_dir in "${arguments[@]}"
    do

        if  [ -f "$dotfile_location/$selected_dir/$dotdotdot_file" ]
        then
            bash "$dotfile_location/$selected_dir/$dotdotdot_file" -i
        else
            printf "File '%s/%s' does not exist\n" \
                "$selected_dir" "$dotdotdot_file"
            exit
        fi

    done

    return 0
}


function update_dotfiles() {

    # TODO run all
    arguments=("$@")
    unset "arguments[0]"

    for selected_dir in "${arguments[@]}"
    do

        if  [ -f "$dotfile_location/$selected_dir/$dotdotdot_file" ]
        then
            bash "$dotfile_location/$selected_dir/$dotdotdot_file" -u
        else
            printf "File '%s/%s' does not exist\n" \
                "$selected_dir" "$dotdotdot_file"
            exit
        fi

    done

    return 0
}


# Create Template function {{{
function create_template {

    arguments=("$@")
    unset "arguments[0]"
    # TODO validate and check there are arguments given

    for selected_dir in "${arguments[@]}"
    do

        # Create the template for dots.sh to create {{{
        template="#!/usr/bin/env bash

# Set location to store dotfiles
dotfile_location=\"\$HOME/$dotfile_local_location/$selected_dir\"

function update() {
    # Commands to run to copy your dotfiles to the repo
    return 0
}

function install() {
    # Commands to run to copy your dotfiles from the repo
    return 0
}

# Do not modify the following code
case \$1 in
    -u|--update)
        update
        ;;
    -i|--install)
        install
        ;;
    *)
        return 1
        ;;
esac
" # }}}

        if [ -f "$selected_dir/$dotdotdot_file" ]
        then
            printf "A '%s' file already exists in '%s'\n" \
                "$dotdotdot_file" "$selected_dir"
            read -r -p "Overwrite existing file (y/n)? " choice
            case "$choice" in
                y|Y|[yY][eE][sS] )
                    echo "$template" > \
                        "$dotfile_location/$selected_dir/$dotdotdot_file"
                    printf "File '%s' in '%s' overwritten\n" \
                        "$dotdotdot_file" "$selected_dir"
                    ;;
                n|N|[nN][oO] )
                    printf "Cancelled overwrite of '%s' in '%s'\n" \
                        "$dotdotdot_file" "$selected_dir"
                    ;;
                * )
                    echo "Invalid option"
                    exit
                    ;;
            esac
        else
            if [ ! -d "$dotfile_location/$selected_dir" ]
            then
                printf "Directory '%s' doesn't exist\n" "$selected_dir"
                read -r -p "Create directory (y/n)? " choice
                case "$choice" in
                    y|Y|[yY][eE][sS] )
                        mkdir -p "$dotfile_location/$selected_dir"
                        printf "Directory '%s' created\n" "$selected_dir"
                        echo "$template" > \
                            "$dotfile_location/$selected_dir/$dotdotdot_file"
                        printf "Template '%s' created in '%s'\n" \
                            "$dotdotdot_file" "$selected_dir"
                        ;;
                    n|N|[nN][oO] )
                        printf "Cancelled creation of directory '%s'\n" \
                            "$selected_dir"
                        ;;
                    * )
                        echo "Invalid option"
                        exit
                        ;;
                esac
            else
                echo "$template" > \
                    "$dotfile_location/$selected_dir/$dotdotdot_file"
                printf "Template '%s' created in '%s'\n" \
                    "$dotdotdot_file" "$selected_dir"
            fi
        fi
    done
    return 0
} # }}}


function print_message() {
    usage_message="usage: dots <option> [dotfile name]"

    if [ "$1" = "help" ]
    then
        printf "
Simple Dotfile Management Script:

%s

Options:
    -h, --help                  Display this help message.
    -t, --template [directory]  Create a management script template.
    -u, --update   [directory]  Update the files in the dotfiles dir.
    -i, --install  [directory]  Install the dotfiles to the system.
\n" "$usage_message"
    elif [ "$1" = "usage" ]
    then
        echo "$usage_message"
    else
        return 1
    fi
}


case $1 in
    -t|--template )
        create_template "$@"
        exit
        ;;
    -u|--update )
        update_dotfiles "$@"
        exit
        ;;
    -i|--install )
        install_dotfiles "$@"
        exit
        ;;
    -h|--help )
        print_message "help"
        exit
        ;;
    * )
        print_message "usage"
        exit
        ;;
esac


# vim: set ts=8 sw=4 tw=80 et ft=sh fdm=marker fmr={{{,}}} :
