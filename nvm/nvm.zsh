export NVM_DIR="$HOME/.nvm"
# source /usr/local/opt/nvm/nvm.sh

nvm() {
  echo "🚨 NVM not loaded! Loading now..."
  unset -f nvm
  export NVM_PREFIX=$(brew --prefix nvm)
  [ -s "$NVM_PREFIX/nvm.sh" ] && . "$NVM_PREFIX/nvm.sh"  nvm "$@"
}
