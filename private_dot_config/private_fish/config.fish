alias rm='rm -i'
alias mv='mv -i'
alias cp='cp -i'
alias e='emacsclient -n'

set -x TERM xterm-256color
set -x LANG en_US.UTF-8
set -x PAGER less
set -x EDITOR vim
set -x EMAIL quentin.stievenart@gmail.com
set -x PATH $PATH $HOME/.local/bin
set -x CHROME_BIN chromium
set -x XDG_CONFIG_HOME $HOME/.config

set -x XMODIFIERS
set -x fish_greeting

# Replace NBSP by normal space
function __fish_space
  commandline -i ' '
end
bind ' ' __fish_space

if status is-interactive
    # Commands to run in interactive sessions can go here
end
