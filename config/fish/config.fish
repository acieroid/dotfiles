# Aliases
alias ls="ls -FG --color=tty"
alias rm="rm -i"
alias mv="mv -i"
alias cp="cp -i"
alias e="emacsclient -n"
alias vim="nvim"
alias cat='bat --style="header" --paging=never'
alias catn='bat --pager "less -RF"'
alias ls="exa --icons"
alias la="exa -lagh --icons"
alias lt="exa -a --tree --icons --level=2"

# Exports
set -x TERM xterm
set -x LANG en_US.UTF-8
set -x PAGER less
set -x EDITOR nvim
set -x EMAIL quentin.stievenart@gmail.com
set -x PATH $PATH $HOME/bin $HOME/.cargo/bin $HOME/.npm-global/bin $HOME/.local/bin
set -x XKB_DEFAULT_LAYOUT fr
set -x XKB_DEFAULT_VARIANT bepo
set -x XKB_DEFAULT_OPTIONS ctrl:nocaps,ctrl:swap_lshift_ctrl
set -x GDK_DPI_SCALE 1
set -x MOZ_ENABLE_WAYLAND 1
set -x XDG_CURRENT_DESKTOP sway
set -x _JAVA_AWT_WM_NOREPARENTING 1
set -x QT_QPA_PLATFORM wayland
set -x QT_WAYLAND_DISABLE_WINDOWDECORATION 1
set -x BEMENU_BACKEND wayland
set -x JAVA_HOME /usr/lib/jvm/default/
set -x CHROME_BIN chromium

# Replace NBSP by normal space
function __fish_space
  commandline -i ' '
end
bind ' ' __fish_space

# Prompt
function fish_prompt
    echo -n -s (set_color $fish_color_cwd) (prompt_pwd) (set_color normal) " \$ "
end

switch (tty)
case /dev/tty1
  sway
  exit 0
case '*'
end

wmname LG3D

# opam configuration
#source /home/quentin/.opam/opam-init/init.fish > /dev/null 2> /dev/null; or true
eval (opam env)


# Wasmer
export WASMER_DIR="/home/quentin/.wasmer"
[ -s "$WASMER_DIR/wasmer.sh" ] && source "$WASMER_DIR/wasmer.sh"

export NETLIFY_AUTH_TOKEN="Y5srXz2pRBUoLzeq42pMkghXnI3szLmTuX7K-NN8gAc"


# atuin init fish | source