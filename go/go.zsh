# Initialize GVM
[[ -s "/Users/mark/.gvm/scripts/gvm" ]] && source "/Users/mark/.gvm/scripts/gvm"

export GOPATH=$HOME/.go
export PATH="${GOPATH//://bin:}/bin:$PATH"
