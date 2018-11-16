#!/bin/sh

# Require homebrew to be installed
command -v brew >/dev/null 2>&1 || { echo >&2 "Homebrew is not installed. Install it from https://brew.sh. Exiting."; exit 1; }

# Run OSX install script
./osx/set-defaults.sh

# Install using the Brewfile
brew bundle

# change to zsh after we verify it's installed
command -v zsh >/dev/null 2>&1 || { echo >&2 "zsh is not installed. Exiting."; exit 1; }

chsh -s zsh

# Link up all the required files.
# zshrc comes first
ln -sf ./zsh/zshrc.symlink "$HOME/.zshrc"
# git
ln -sf ./git/gitconfig.symlink "$HOME/.gitconfig"
ln -sf ./git/gitignore.symlink "$HOME/.gitignore"
# gpg
ln -sf ./gpg/gpg-agent.conf.symlink "$HOME/.gnupg/gpg-agent.conf"
ln -sf ./gpg/gpg.conf.symlink "$HOME/.gnupg/gpg.conf"
# intellij
ln -sf ./intellij/ideavimrc.symlink "$HOME/.ideavimrc"
