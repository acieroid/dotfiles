#!/bin/sh -x
mkdir -p ~/.config

# XDG-compliant programs
ln -s $(pwd)/git ~/.config/
ln -s $(pwd)/nvim ~/.config/
ln -s $(pwd)/i3 ~/.config/
ln -s $(pwd)/fish ~/.config/
ln -s $(pwd)/tridactyl ~/.config/

ln -s $(pwd)/bin ~/bin

# Others
ln -s $(pwd)/mostrc ~/.mostrc
ln -s $(pwd)/emacs ~/.emacs
ln -s $(pwd)/tmux.conf ~/.tmux.conf
ln -s $(pwd)/XCompose ~/.XCompose
ln -s $(pwd)/XCompose.math ~/.XCompose.math


sudo ln -s $(pwd)/configuration.nix /etc/nixos/configuration.nix
# TODO: xmodmap, vimperator
