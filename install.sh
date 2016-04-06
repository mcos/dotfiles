#!/bin/sh

# TODO: Clean these up
ln -f ./zsh/zshrc.symlink ~/.zshrc

# Powerline Theme
ln -f ./themes/mark.zsh-theme ~/.oh-my-zsh/themes/mark.zsh-theme
ln -f ./themes/mark-bira.zsh-theme ~/.oh-my-zsh/themes/mark-bira.zsh-theme

# Run OSX install script
./osx/set-defaults.sh
