#!/bin/sh

# Require homebrew to be installed
command -v brew >/dev/null 2>&1 || { echo >&2 "I require homebrew but it's not installed. Install it from https://brew.sh. Aborting."; exit 1; }

# Run OSX install script
./osx/set-defaults.sh

# Install using the Brewfile
brew bundle

# TODO: Clean these up
ln -f ./zsh/zshrc.symlink ~/.zshrc
