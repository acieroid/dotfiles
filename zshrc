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
alias i='ssh -C acieroid@awesom.eu -p 42022 -t screen -Udr'
alias s='ssh -C admin@awesom.eu -p 25022'
alias h='ssh -C acieroid@ks -p 21022'
alias m='ssh -C acieroid@foo.awesom.eu -p 42022 -t screen -Udr'

# variables
export PAGER=most
export EMAIL=acieroid@awesom.eu
export PATH=$PATH:/usr/sbin:/sbin:$HOME/bin
export HISTFILE=~/.history
export HISTSIZE=10000
export SAVEHIST=$HISTSIZE
export WORDCHARS=${WORDCHARS:s,/,,} # to have an emacs-like backward-kill-word

# terminal name
if [ $TERM = "rxvt-unicode" ]; then
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
  local dircol="%{${fg_no_bold[green]}%}"
  local sign="$"
fi

export PS1="${dircol}%}%~%{${reset_color}%} ${sign} "
export PS2="... "
export RPS1="`hostname`"
}
export TERM=rxvt
export XDG_CONFIG_HOME=~/.config/
