#!/usr/bin/env bash


# Complete theme selection

function select_theme() {

    printf "
Select a theme to load:
    [1] : Default theme,
    [2] : Adapta theme,
    [3] : Adapta-Eta theme,
    [4] : Arc theme,
    [0] : Back / Cancel.

    Selection: "

        read -r theme
        printf "\n"

        if [ "$theme" = "1" ]
        then
            default_theme
            theme_message="Default theme loaded"

        elif [ "$theme" = "2" ]
        then
            adapta_theme
            theme_message="Adapta theme loaded"

        elif [ "$theme" = "3" ]
        then
            adapta-eta_theme
            theme_message="Adapta-Eta theme loaded"

        elif [ "$theme" = "4" ]
        then
            arc_theme
            theme_message="Arc theme loaded"

        elif [ "$theme" = "0" ]
        then
            exit_theme_menu=1

        else
            # Error message
            theme_message="ERROR: \"$theme\" is not a valid option"

        fi

    }


    function setup_for_theme() {    # ONLY RUN ONCE
        # Only run once to avoid errors, and breaking the extension
        gnome-shell-ext-conf -e 'user-theme@gnome-shell-extensions.gcampax.github.com'
    }


    function default_theme() {

        # Theme configuration
        gsettings set org.gnome.desktop.interface cursor-theme   'Adwaita'
        gsettings set org.gnome.desktop.interface icon-theme     'Adwaita'
        gsettings set org.gnome.desktop.interface gtk-theme      'Adwaita'
        gsettings set org.gnome.shell.extensions.user-theme name 'Adwaita'

        # Font configuration
        gsettings set org.gnome.desktop.wm.preferences titlebar-font  'Cantarell 11'
        gsettings set org.gnome.desktop.interface font-name           'Cantarell Bold 11'
        gsettings set org.gnome.desktop.interface document-font-name  'Sans 11'
        gsettings set org.gnome.desktop.interface monospace-font-name 'Monospace 11'

        # Extensions configuration
        gnome-shell-ext-conf -da
        gnome-shell-ext-conf -e 'background-logo@fedorahosted.org'

        # Wallpaper
        gsettings set org.gnome.desktop.background picture-uri \
            "file:///usr/share/backgrounds/f$fedora_version/default/f$fedora_version.xml"
        gsettings set org.gnome.desktop.screensaver picture-uri \
            "file:///usr/share/backgrounds/f$fedora_version/default/f$fedora_version.xml"


    }


    function adapta_theme() {

        # Theme configuration
        gsettings set org.gnome.desktop.interface cursor-theme   'Breeze_Snow'
        gsettings set org.gnome.desktop.interface icon-theme     'Arc'
        gsettings set org.gnome.desktop.interface gtk-theme      'Adapta'
        gsettings set org.gnome.shell.extensions.user-theme name 'Adapta-Nokto'

        # Font configuration
        gsettings set org.gnome.desktop.wm.preferences titlebar-font  'Roboto 10'
        gsettings set org.gnome.desktop.interface font-name           'Roboto Bold 10'
        gsettings set org.gnome.desktop.interface document-font-name  'Sans 10'
        gsettings set org.gnome.desktop.interface monospace-font-name 'Source Code Pro 10'

        # Extensions configuration
        gnome-shell-ext-conf -da
        gnome-shell-ext-conf -e 'dynamic-panel-transparency@rockon999.github.io'
        gnome-shell-ext-conf -e 'dynamic-panel-transparency@rockon999.github.io' # twice to solve unknown bug

        # Wallpaper
        gsettings set org.gnome.desktop.background picture-uri \
            "file:///home/$USER/Pictures/tealized.jpg"
        gsettings set org.gnome.desktop.screensaver picture-uri \
            "file:///home/$USER/Pictures/tealized.jpg"

    }

    function adapta-eta_theme() {

        # Theme configuration
        gsettings set org.gnome.desktop.interface cursor-theme   'Breeze_Snow'
        gsettings set org.gnome.desktop.interface icon-theme     'Arc'
        gsettings set org.gnome.desktop.interface gtk-theme      'Adapta-Eta'
        gsettings set org.gnome.shell.extensions.user-theme name 'Adapta-Nokto-Eta'

        # Font configuration
        gsettings set org.gnome.desktop.wm.preferences titlebar-font  'Roboto 10'
        gsettings set org.gnome.desktop.interface font-name           'Roboto Bold 10'
        gsettings set org.gnome.desktop.interface document-font-name  'Sans 10'
        gsettings set org.gnome.desktop.interface monospace-font-name 'Source Code Pro 10'

        # Extensions configuration
        gnome-shell-ext-conf -da
        gnome-shell-ext-conf -e 'dynamic-panel-transparency@rockon999.github.io'
        gnome-shell-ext-conf -e 'dynamic-panel-transparency@rockon999.github.io' # twice to solve unknown bug

        # Wallpaper
        gsettings set org.gnome.desktop.background picture-uri \
            "file:///home/$USER/Pictures/tealized.jpg"
        gsettings set org.gnome.desktop.screensaver picture-uri \
            "file:///home/$USER/Pictures/tealized.jpg"

    }


    function arc_theme() {

        # Theme configuration
        gsettings set org.gnome.desktop.interface cursor-theme   'Breeze_Snow'
        gsettings set org.gnome.desktop.interface icon-theme     'Arc'
        gsettings set org.gnome.desktop.interface gtk-theme      'Arc-Darker'
        gsettings set org.gnome.shell.extensions.user-theme name 'Arc-Dark'

        # Font configuration
        gsettings set org.gnome.desktop.wm.preferences titlebar-font  'Roboto 10'
        gsettings set org.gnome.desktop.interface font-name           'Roboto Bold 10'
        gsettings set org.gnome.desktop.interface monospace-font-name 'Sans 10'
        gsettings set org.gnome.desktop.interface document-font-name  'Roboto Mono 10'

        # Extensions configuration
        gnome-shell-ext-conf -da
        gnome-shell-ext-conf -e 'dynamic-panel-transparency@rockon999.github.io'
        gnome-shell-ext-conf -e 'dynamic-panel-transparency@rockon999.github.io' # twice to solve unknown bug

        # Wallpaper
        # TODO
        #gsettings set org.gnome.desktop.background picture-uri \
            #          ""
        #gsettings set org.gnome.desktop.screensaver picture-uri \
            #          ""

    }

    # ------------------------------------------------------------------------------


    exit_theme_menu=0
    theme_message="Theme Selection Menu"

    while [ "$exit_theme_menu" != "1" ]
    do
        printf "\n%s\n" "$theme_message"
        select_theme
    done
