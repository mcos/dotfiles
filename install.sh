#!/bin/sh

# TODO: Clean these up
ln -f ./zsh/zshrc.symlink ~/.zshrc

# Atom
ln -f ./atom/atom.coffee ~/.atom/atom.coffee
ln -f ./atom/init.coffee ~/.atom/init.coffee
ln -f ./atom/config.cson ~/.atom/config.cson

# Powerline Theme
ln -f ./themes/mark.zsh-theme ~/.oh-my-zsh/themes/mark.zsh-theme

# Run OSX install script
./osx/set-defaults.sh
