#!/bin/sh


main() {
    cat << EOF
GNOME Theme Manager
-------------------

Select a theme to load:
[1]: GNOME Adwaita theme
[2]: Default Fedora theme
[3]: Default Ubuntu theme
[0]: Exit
EOF
    read -r i
    printf "\n"
    case "$i" in
        1)  adwaita_theme;;
        2)  fedora_theme;;
        3)  ubuntu_theme;;
        0)  return 0;;
        *)  printf "Error: Invalid option.\\n";;
    esac
    main
}


change_theme() {
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


adwaita_theme() {
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
    lock_screen_wallpaper="file:///usr/share/backgrounds/gnome/adwaita-timed.jpg"

    change_theme

    # Extension Configuration
    gnome-shell-extension-cl -da
}


fedora_theme() {
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
    fedora_version=29
    desktop_wallpaper="file:///usr/share/backgrounds/f$fedora_version/default/f$fedora_version.xml"
    lock_screen_wallpaper="file:///usr/share/backgrounds/f$fedora_version/default/f$fedora_version.xml"

    change_theme

    # Extension Configuration
    gnome-shell-extension-cl -da
    gnome-shell-extension-cl -e 'background-logo@fedorahosted.org'
}


ubuntu_theme() {
    #GNOME Shell Theming
    cursor_theme='Yaru'
    icon_theme='Yaru'
    gtk_theme='Yaru'
    shell_theme='Yaru'

    #GNOME Font Theming
    window_titles_font='Ubuntu Bold 11'
    interface_font='Ubuntu 11'
    document_font='Sans 11'
    monospace_font='Ubuntu Mono 13'

    #Wallpaper Theming
    desktop_wallpaper='file:///usr/share/backgrounds/warty-final-ubuntu.png'
    lock_screen_wallpaper='file:///usr/share/backgrounds/warty-final-ubuntu.png'

    change_theme

    # Extension Configuration
    gnome-shell-extension-cl -da
}


# ONLY RUN THIS ONCE
# If running Ubuntu, first install the `gnome-shell-extensions` package
setup_system() {
    sudo wget https://raw.githubusercontent.com/cyberalex4life/gnome-shell-extension-cl/master/gnome-shell-extension-cl \
    -O /usr/local/bin/gnome-shell-extension-cl
    sudo chmod +x /usr/local/bin/gnome-shell-extension-cl
    gnome-shell-extension-cl -e 'user-theme@gnome-shell-extensions.gcampax.github.com'
}


export_theme() {

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


usage() {
    printf "usage: %s [-es]\\n" "$0"
}


while getopts se o "$@"
do
    case "$o" in
        s)      setup_system; exit 0;;
        e)      export_theme; exit 0;;
        i)      exit 0;; # TODO GNOME extension installer?
        u)      usage; exit 0;;
        [?])    usage; exit 1;;
    esac
done


main
