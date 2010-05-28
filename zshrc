# Some useful modules
autoload -U compinit promptinit

# completion
compinit
zstyle ':completion:*:descriptions' format '%U%B%d%b%u'
zstyle ':completion:*:warnings' format 'No results for: %d%b'
zstyle ':completion:*' menu select=2
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s

zstyle ':completion:*:rm:*' ignore-line yes
zstyle ':completion:*:mv:*' ignore-line yes
zstyle ':completion:*:cp:*' ignore-line yes

autoload -U bashcompinit
bashcompinit

# commands corrections 
# really annoying -> deactivate
#setopt correctall

# prompt
autoload colors ; colors
promptinit
export RPROMPT=""
export PS1="%{${fg[green]%}%}%~%{${fg[white]%}%} %% "
export PS2="... "

# some useful aliases
alias ls='ls --color=auto -F -G'
alias ec='emacsclient'
alias irc='ssh -D 1080 -C acieroid@foobar -p 443 -t screen -Udr'
alias grep='grep --color=auto'
alias jamendo='mpc listall | grep Jamendo | cut -d - -f 1 | uniq'
alias mpca='mpc --format "[%artist% - %title% - %album%]|[%file%]"'
alias rm='rm -I'

# History
export HISTSIZE=1000 
export HISTFILE="$HOME/.history"
export SAVEHIST=$HISTSIZE
setopt hist_ignore_all_dups # Avoid same commands
setopt hist_ignore_space # Don't save if the first char is space

# extended globs
setopt extendedglob

# turn of the beep
setopt nobeep

# emacs like keybindings
bindkey -e

# Turn off line editor in emacs (uses emacs shell mode's instead)
[[ $EMACS = t ]] && unsetopt zle

# download a album from jamendo (doesn't seem to work anymore)
function jamendl {
  wget "http://www.jamendo.com/get/album/id/album/archiverestricted/redirect/$1/?p2pnet=bittorrent&are=ogg3"
}

# some variables more or less usefull
export EDITOR=vim
export PAGER=most
export EMAIL=acieroid@awesom.eu
export PATH=$PATH:/usr/sbin/:/sbin/:~/bin:/usr/pkg/bin:/usr/pkg/sbin:/usr/local/bin
export LANG="en_US.UTF-8"
export LC_ALL=$LANG
export TERM=rxvt
export GDFONTPATH=/usr/share/fonts/TTF/
export BROWSER="elinks"

export CLOJURE_HOME=/var/abs/local/clj/clojure
export CLJ_CLASSPATH=.:/var/abs/local/clj/clojure/clojure.jar:/usr/share/clojure/clojure-contrib.jar
export ERLANG_HOME=/home/quentin/pkg/erlang
export PATH=$PATH:/home/quentin/pkg/erlang/nitrogen/support

sudo() { su -c "$*" }
