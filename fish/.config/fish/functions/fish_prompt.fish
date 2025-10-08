function fish_prompt
    set -l normal (set_color normal)
    set -l suffix '%' # $ @ ? -> : ><)>
    echo -n -s \
        (set_color $fish_color_cwd) (prompt_pwd) \
        (fish_git_prompt " %s") " " \
        $normal $suffix " "
end
