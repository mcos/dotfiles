# Initialize GVM
[[ -s "/Users/mark/.gvm/scripts/gvm" ]] && source "/Users/mark/.gvm/scripts/gvm"

export GOPATH=$HOME/.go
export GOROOT=`go env GOROOT`
export PATH="${GOPATH//://bin:}/bin:${GOROOT//://bin:}/bin:$PATH"
