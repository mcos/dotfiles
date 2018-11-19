git_info() {
  # Exit if not inside a Git repository
  ! git rev-parse --is-inside-work-tree > /dev/null 2>&1 && return

  # Git branch/tag, or name-rev if on detached head
  local GIT_LOCATION=${$(git symbolic-ref -q HEAD || git name-rev --name-only --no-undefined --always HEAD)#(refs/heads/|tags/)}

  DIRTY=""
  if [[ $(git diff --shortstat 2> /dev/null | tail -n1) != "" ]]; then
    DIRTY="*"
  elif [[ $(git status --porcelain -unormal 2> /dev/null) ]]; then
    DIRTY="*"
  fi

  echo "\uE0A0 $GIT_LOCATION$DIRTY"
}

local git_state='$(git_info)'

PROMPT="%m %B%~%b ${git_state}
%B❯%b "
