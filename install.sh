#!/bin/sh

# admin privileges are needed for this
sudo -v

# Keep-alive: update existing `sudo` time stamp until we have finished
while true; do sudo -n true; sleep 60; kill -0 "$$" || exit; done 2>/dev/null &

# Require homebrew to be installed
command -v brew >/dev/null 2>&1 || { echo >&2 "Homebrew is not installed. Install it from https://brew.sh. Exiting."; exit 1; }

# Run macos install script
./macos/set-defaults.sh

# Install homebrew casks and packages using the Brewfile
brew bundle

# Switch to zsh
ZSH="$(which zsh)"
sudo grep -qF "$ZSH" /etc/shells || echo "$ZSH"| sudo tee -a /etc/shells
chsh -s "$(which zsh)"

# Link up all the required files.
# zshrc comes first
ln -sf $PWD/zsh/zshrc.symlink "$HOME/.zshrc"
# git
ln -sf $PWD/git/gitconfig.symlink "$HOME/.gitconfig"
ln -sf $PWD/git/gitignore.symlink "$HOME/.gitignore"
# gpg
mkdir -p "$HOME/.gnupg"
ln -sf $PWD/gpg/gpg-agent.conf.symlink "$HOME/.gnupg/gpg-agent.conf"
ln -sf $PWD/gpg/gpg.conf.symlink "$HOME/.gnupg/gpg.conf"
# intellij
ln -sf $PWD/intellij/ideavimrc.symlink "$HOME/.ideavimrc"

# vscode
mkdir -p "$HOME/Library/Application\ Support/Code/User"
ln -sf $PWD/vscode/settings.json "$HOME/Library/Application\ Support/Code/User/settings.json"
ln -sf $PWD/vscode/keybindings.json "$HOME/Library/Application\ Support/Code/User/keybindings.json"
ln -sf $PWD/vscode/snippets "$HOME/Library/Application\ Support/Code/User/"
## install vscode extensions. `vscode/extensions.list` is generated via a `code --list-extensions` call.
cat $PWD/vscode/extensions.list | grep -v '^#' | xargs -L1 code --install-extension
