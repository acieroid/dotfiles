alias rm='rm -i'
alias mv='mv -i'
alias cp='cp -i'
alias e='emacsclient -n'
alias vim='nvim'
alias v='nvim'

set -x TERM xterm
set -x LANG en_US.UTF-8
set -x PAGER less
set -x EDITOR nvim
set -x EMAIL quentin.stievenart@gmail.com
set -x PATH $PATH $HOME/.local/bin
set -x CHROME_BIN chromium

set -x fish_greeting

# Replace NBSP by normal space
function __fish_space
  commandline -i ' '
end
bind ' ' __fish_space

if status is-interactive
    # Commands to run in interactive sessions can go here
end

nvm --silent use v16

set -q GHCUP_INSTALL_BASE_PREFIX[1]; or set GHCUP_INSTALL_BASE_PREFIX $HOME ; set -gx PATH $HOME/.cabal/bin $PATH /home/quentin/.ghcup/bin # ghcup-env

eval (opam env)
