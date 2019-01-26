# ~/.bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
    . /etc/bashrc
fi

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# History config
export HISTSIZE=10000
export HISTFILESIZE=20000
export HISTCONTROL=ignoredups:erasedups
shopt -s histappend

# Set locale and language
export LANG=en_GB.UTF-8
# export LANG=en_US.UTF-8
# export LC_ALL=POSIX

# Bash improvements
shopt -s globstar checkwinsize

# Set default editor
export VISUAL=vim
export EDITOR=vim

# .NET Core settings
export ASPNETCORE_ENVIRONMENT=Development
export DOTNET_CLI_TELEMETRY_OPTOUT=1


# == Bash Prompts ==

# Simple prompt
#PS1="[\u@\h \W]\$ "
PS2="> "

# Prompt with Git branch
git_branch() {
    branch=$(git branch 2> /dev/null | grep "^\*" | sed 's/^\* //')
    [ -n "$branch" ] && printf " $branch"
}
PS1="[\u@\h \W\[\e[0;32m\]\`git_branch\`\[\e[0;00m\]]\$ "

# Small prompt for Android
[ "$(uname -o)" == "Android" ] && PS1="[\W]\$ "


# == Aliases & Functions ==

alias ls="ls --color=auto"
alias grep="grep --color=auto"
alias egrep="egrep --color=auto"
alias fgrep="fgrep --color=auto"

alias ledger="ledger -f ${HOME}/.ledger/personal.dat"
alias ledger-record="${EDITOR} ${HOME}/.ledger/personal.dat"
