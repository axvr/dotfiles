#!/usr/bin/env bash

# Arguments:
#   setup  - Set up system to be able to use this script
#   export - print current configuration (can be copied and pasted into this script)

# NOTE: User installed Shell extensions are stored in:
#       "~/.local/share/gnome-shell/extensions/"


function setup_for_theme_switching() {    # ONLY RUN ONCE (using 'setup' as an option)
    sudo wget https://raw.githubusercontent.com/cyberalex4life/gnome-shell-extension-cl/master/gnome-shell-extension-cl \
    -O /usr/local/bin/gnome-shell-extension-cl
    sudo chmod +x /usr/local/bin/gnome-shell-extension-cl
    gnome-shell-extension-cl -e 'user-theme@gnome-shell-extensions.gcampax.github.com'
}


function export_current_theme() {

    # Theme configuration
    printf "#GNOME Shell Theming\n"
    printf "cursor_theme="
    gsettings get org.gnome.desktop.interface cursor-theme
    printf "icon_theme="
    gsettings get org.gnome.desktop.interface icon-theme
    printf "gtk_theme="
    gsettings get org.gnome.desktop.interface gtk-theme
    printf "shell_theme="
    gsettings get org.gnome.shell.extensions.user-theme name

    # Font configuration
    printf "\n#GNOME Font Theming\n"
    printf "window_titles_font="
    gsettings get org.gnome.desktop.wm.preferences titlebar-font
    printf "interface_font="
    gsettings get org.gnome.desktop.interface font-name
    printf "document_font="
    gsettings get org.gnome.desktop.interface document-font-name
    printf "monospace_font="
    gsettings get org.gnome.desktop.interface monospace-font-name

    # Wallpapers
    printf "\n#Wallpaper Theming\n"
    printf "desktop_wallpaper="
    gsettings get org.gnome.desktop.background picture-uri
    printf "lock_screen_wallpaper="
    gsettings get org.gnome.desktop.screensaver picture-uri

    printf "\nchange_theme\n"

    # Extension configuration
    printf "\n# Extension Configuration\n"
    printf "gnome-shell-extension-cl -da\n"
    for extension in $(gnome-shell-extension-cl -le)
    do
        if [ "$extension" != "user-theme" ] && [ "$extension" != "user-themes" ] && \
            [ "$extension" != "user-theme@gnome-shell-extensions.gcampax.github.com" ]
        then
            printf "gnome-shell-extension-cl -e '%s'\n"  "$extension"
        fi
    done

}


function change_theme() {

    # Theme configuration
    gsettings set org.gnome.desktop.interface cursor-theme      "$cursor_theme"
    gsettings set org.gnome.desktop.interface icon-theme        "$icon_theme"
    gsettings set org.gnome.desktop.interface gtk-theme         "$gtk_theme"
    gsettings set org.gnome.shell.extensions.user-theme name    "$shell_theme"

    # Font configuration
    gsettings set org.gnome.desktop.wm.preferences titlebar-font    "$window_titles_font"
    gsettings set org.gnome.desktop.interface font-name             "$interface_font"
    gsettings set org.gnome.desktop.interface document-font-name    "$document_font"
    gsettings set org.gnome.desktop.interface monospace-font-name   "$monospace_font"

    # Wallpaper
    gsettings set org.gnome.desktop.background picture-uri  "$desktop_wallpaper"
    gsettings set org.gnome.desktop.screensaver picture-uri "$lock_screen_wallpaper"

}


function select_theme() {

    while true
    do
        printf "
Select a theme to load:
[1] : Default GNOME theme
[2] : Default Fedora theme,
[3] : Adapta theme,
[4] : Adapta-Eta theme,
[5] : Arc theme,
[0] : Back / Cancel.

Selection: "

        read -r theme
        printf "\n"

        case $theme in
            1)
                # GNOME Shell Theming
                cursor_theme="Adwaita"
                icon_theme="Adwaita"
                gtk_theme="Adwaita"
                shell_theme="Adwaita"

                # GNOME Font Theming
                window_titles_font="Cantarell 11"
                interface_font="Cantarell Bold 11"
                document_font="Sans 11"
                monospace_font="Monospace 11"

                # Wallpaper Theming
                desktop_wallpaper="file:///usr/share/backgrounds/gnome/adwaita-timed.xml"
                lock_screen_wallpaper="file:///usr/share/backgrounds/gnome/adwaita-lock.jpg"

                change_theme

                # Extension Configuration
                gnome-shell-extension-cl -da

                ;;
            2)
                # GNOME Shell Theming
                cursor_theme="Adwaita"
                icon_theme="Adwaita"
                gtk_theme="Adwaita"
                shell_theme="Adwaita"

                # GNOME Font Theming
                window_titles_font="Cantarell 11"
                interface_font="Cantarell Bold 11"
                document_font="Sans 11"
                monospace_font="Monospace 11"

                # Wallpaper Theming
                fedora_version=27
                desktop_wallpaper="file:///usr/share/backgrounds/f$fedora_version/default/f$fedora_version.xml"
                lock_screen_wallpaper="file:///usr/share/backgrounds/f$fedora_version/default/f$fedora_version.xml"

                change_theme

                # Extension Configuration
                gnome-shell-extension-cl -da
                gnome-shell-extension-cl -e 'background-logo@fedorahosted.org'

                ;;
            3)
                # GNOME Shell Theming
                cursor_theme="Breeze_Snow"
                icon_theme="Arc"
                gtk_theme="Adapta"
                shell_theme="Adapta-Nokto"

                # GNOME Font Theming
                window_titles_font="Roboto 10"
                interface_font="Roboto Bold 10"
                document_font="Sans 10"
                monospace_font="Source Code Pro 10"

                # Wallpaper Theming
                desktop_wallpaper="file://$HOME/Pictures/tealized.jpg"
                lock_screen_wallpaper="file://$HOME/Pictures/tealized.jpg"

                change_theme

                # Extension Configuration
                gnome-shell-extension-cl -da

                ;;
            4)
                # GNOME Shell Theming
                cursor_theme="Breeze_Snow"
                icon_theme="Arc"
                gtk_theme="Adapta-Eta"
                shell_theme="Adapta-Nokto-Eta"

                # GNOME Font Theming
                window_titles_font="Roboto 10"
                interface_font="Roboto Bold 10"
                document_font="Sans 10"
                monospace_font="Source Code Pro 10"

                # Wallpaper Theming
                desktop_wallpaper="file://$HOME/Pictures/tealized.jpg"
                lock_screen_wallpaper="file://$HOME/Pictures/tealized.jpg"

                change_theme

                # Extension Configuration
                gnome-shell-extension-cl -da

                ;;
            5)
                # GNOME Shell Theming
                cursor_theme="Breeze_Snow"
                icon_theme="Arc"
                gtk_theme="Arc-Darker"
                shell_theme="Arc-Dark"

                # GNOME Font Theming
                window_titles_font="Roboto 10"
                interface_font="Roboto Bold 10"
                document_font="Sans 10"
                monospace_font="Roboto Mono 10"

                # Wallpaper Theming
                desktop_wallpaper="file://$HOME/Pictures/mountains_garrett_parker.jpg"
                lock_screen_wallpaper="file://$HOME/Pictures/mountains_garrett_parker.jpg"

                change_theme

                # Extension Configuration
                gnome-shell-extension-cl -da
                # enable twice to solve unknown bug
                #gnome-shell-extension-cl -e 'dynamic-panel-transparency@rockon999.github.io'
                #gnome-shell-extension-cl -e 'dynamic-panel-transparency@rockon999.github.io'
                # ^ disabled because now translucency is default in GNOME

                ;;
            0)
                exit ;;
            *)
                printf "ERROR: '%s' is not a valid option\n" "$theme"
                continue
                ;;
        esac
    done
}



case ${1,,} in
    setup)
        setup_for_theme_switching ;;
    export)
        export_current_theme ;;
    *)
        select_theme ;;
esac
