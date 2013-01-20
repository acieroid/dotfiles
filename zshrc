
# completion
autoload -U compinit
compinit
autoload -U bashcompinit
bashcompinit

zstyle ':completion:*:descriptions' format '%B%d%b%u'
zstyle ':completion:*:warnings' format '%BNo results for %d%b'
zstyle ':completion:*' menu select=2
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*:rm:*' ignore-line yes
zstyle ':completion:*:mv:*' ignore-line yes
zstyle ':completion:*:cp:*' ignore-line yes
zstyle ':completion:*:processes' command 'ps aux | grep $USER'
zstyle ':completion:*:*:kill:*:processes' menu yes select
zstyle ':completion:*:kill:*' force-list always
zstyle ':completion:*:cd:*' ignore-parents parent pwd

# aliases
case `uname -s` in
  FreeBSD) alias ls='ls -FG';;
  Linux) alias ls='ls -F --color=auto';;
esac
alias grep='grep --color=auto'
alias rm='rm -I'
alias mv='mv -i'
alias cp='cp -i'
alias i='ssh -p 443 -t acieroid@awesom.eu screen -Udr'
alias s='ssh -C admin@awesom.eu -p 25022'
alias h='ssh -C acieroid@ks -p 21022'
alias m='ssh -C acieroid@foo.awesom.eu -p 42022'

# variables
export LANG=en_US.UTF-8
export PAGER=most
export EDITOR=vim
export EMAIL=quentin.stievenart@gmail.com
export PATH=$PATH:/usr/sbin:/sbin:$HOME/bin
export HISTFILE=~/.zsh_history
export HISTSIZE=10000
export SAVEHIST=$HISTSIZE
export WORDCHARS=${WORDCHARS:s,/,,} # to have an emacs-like backward-kill-word
export XDG_CONFIG_HOME=~/.config/
export GTK_IM_MODULE="xim"
export GDFONTPATH=/usr/local/lib/X11/fonts/dejavu:/usr/local/lib/X11/fonts/Droid/
export PKG_CONFIG_PATH=/usr/local/lib/pkgconfig
export MPD_HOST=192.168.2.232

if [ -z "$DISPLAY" ]; then
  export DISPLAY=:0.0
fi


# terminal name
if [ `echo $TERM | wc -c` -gt 8 ]; then
  export TERM="rxvt"
fi

# settings
setopt hist_ignore_space
setopt hist_reduce_blanks
setopt hist_ignore_all_dups
setopt nohup
setopt transient_rprompt
setopt append_history

# prompt
autoload colors
colors

function precmd {

if [ `id -u` -eq 0 ]; then
  local dircol="%{${fg_no_bold[red]}%}"
  local sign="#"
else
  local dircol
  if [ `uname -m` = 'armv6l' ]; then
   dircol="%{${fg_no_bold[blue]}%}"
  else
   dircol="%{${fg_no_bold[green]}%}"
  fi
  local sign="$"
fi

export PS1="${dircol}%}%~%{${reset_color}%} ${sign} "
export PS2="... "
export RPS1=""
}

# emacs-like keybindings
bindkey -e

# bind the nbsp on a normal space
function space {
  LBUFFER+=" "
  zle self-insert
}
zle -N space
bindkey " " space

# bind the delete key to delete-char
bindkey    "^[[3~"          delete-char
bindkey    "^[3;5~"         delete-char
