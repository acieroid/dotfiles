# Aliases
alias ls="ls -FG --color=tty"
alias rm="rm -i"
alias mv="mv -i"
alias cp="cp -i"
alias e="emacsclient -n"
alias vim="nvim"

# Exports
set -x LANG en_US.UTF-8
set -x PAGER most
set -x EDITOR nvim
set -x EMAIL quentin.stievenart@gmail.com
set -x PATH $PATH $HOME/bin

# Replace NBSP by normal space
function __fish_space
  commandline -i ' '
end
bind ' ' __fish_space

# Prompt
function fish_prompt
    echo -n -s (set_color $fish_color_cwd) (prompt_pwd) (set_color normal) " \$ "
end

