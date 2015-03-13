setprompt () {

  if [ "$POWERLINE_DATE_FORMAT" = "" ]; then
    POWERLINE_DATE_FORMAT=%D{%Y-%m-%d}
  fi

  if [ "$POWERLINE_RIGHT_B" = "" ]; then
    POWERLINE_RIGHT_B=%D{%H:%M:%S}
  elif [ "$POWERLINE_RIGHT_B" = "none" ]; then
    POWERLINE_RIGHT_B=""
  fi

  if [ "$POWERLINE_RIGHT_A" = "mixed" ]; then
    POWERLINE_RIGHT_A=%(?."$POWERLINE_DATE_FORMAT".%F{red}✘ %?)
  elif [ "$POWERLINE_RIGHT_A" = "exit-status" ]; then
    POWERLINE_RIGHT_A=%(?.%F{green}✔ %?.%F{red}✘ %?)
  elif [ "$POWERLINE_RIGHT_A" = "date" ]; then
    POWERLINE_RIGHT_A="$POWERLINE_DATE_FORMAT"
  fi

  if [ "$POWERLINE_HIDE_USER_NAME" = "" ] && [ "$POWERLINE_HIDE_HOST_NAME" = "" ]; then
      POWERLINE_USER_NAME="%n@%m"
  elif [ "$POWERLINE_HIDE_USER_NAME" != "" ] && [ "$POWERLINE_HIDE_HOST_NAME" = "" ]; then
      POWERLINE_USER_NAME="@%m"
  elif [ "$POWERLINE_HIDE_USER_NAME" = "" ] && [ "$POWERLINE_HIDE_HOST_NAME" != "" ]; then
      POWERLINE_USER_NAME="%n"
  else
      POWERLINE_USER_NAME=""
  fi

  POWERLINE_CURRENT_PATH="%d"

  if [ "$POWERLINE_FULL_CURRENT_PATH" = "" ]; then
    POWERLINE_CURRENT_PATH="%~"
  fi

  if [ "$POWERLINE_GIT_CLEAN" = "" ]; then
    POWERLINE_GIT_CLEAN="✔"
  fi

  if [ "$POWERLINE_GIT_DIRTY" = "" ]; then
    POWERLINE_GIT_DIRTY="✘"
  fi

  if [ "$POWERLINE_GIT_ADDED" = "" ]; then
    POWERLINE_GIT_ADDED="%F{green}✚%F{black}"
  fi

  if [ "$POWERLINE_GIT_MODIFIED" = "" ]; then
    POWERLINE_GIT_MODIFIED="%F{blue}✹%F{black}"
  fi

  if [ "$POWERLINE_GIT_DELETED" = "" ]; then
    POWERLINE_GIT_DELETED="%F{red}✖%F{black}"
  fi

  if [ "$POWERLINE_GIT_UNTRACKED" = "" ]; then
    POWERLINE_GIT_UNTRACKED="%F{yellow}✭%F{black}"
  fi

  if [ "$POWERLINE_GIT_RENAMED" = "" ]; then
    POWERLINE_GIT_RENAMED="➜"
  fi

  if [ "$POWERLINE_GIT_UNMERGED" = "" ]; then
    POWERLINE_GIT_UNMERGED="═"
  fi

  ZSH_THEME_GIT_PROMPT_PREFIX=" \ue0a0 "
  ZSH_THEME_GIT_PROMPT_SUFFIX=""
  ZSH_THEME_GIT_PROMPT_DIRTY=" $POWERLINE_GIT_DIRTY"
  ZSH_THEME_GIT_PROMPT_CLEAN=" $POWERLINE_GIT_CLEAN"

  ZSH_THEME_GIT_PROMPT_ADDED=" $POWERLINE_GIT_ADDED"
  ZSH_THEME_GIT_PROMPT_MODIFIED=" $POWERLINE_GIT_MODIFIED"
  ZSH_THEME_GIT_PROMPT_DELETED=" $POWERLINE_GIT_DELETED"
  ZSH_THEME_GIT_PROMPT_UNTRACKED=" $POWERLINE_GIT_UNTRACKED"
  ZSH_THEME_GIT_PROMPT_RENAMED=" $POWERLINE_GIT_RENAMED"
  ZSH_THEME_GIT_PROMPT_UNMERGED=" $POWERLINE_GIT_UNMERGED"
  ZSH_THEME_GIT_PROMPT_AHEAD=" ⬆"
  ZSH_THEME_GIT_PROMPT_BEHIND=" ⬇"
  ZSH_THEME_GIT_PROMPT_DIVERGED=" ⬍"

  # if [ "$POWERLINE_SHOW_GIT_ON_RIGHT" = "" ]; then
  #     if [ "$POWERLINE_HIDE_GIT_PROMPT_STATUS" = "" ]; then
  #         POWERLINE_GIT_INFO_LEFT=" %F{white}%K{178}"$'\ue0b0'"%F{178}%F{black}%K{178}"$'$(git_prompt_info)$(git_prompt_status)%F{178}'
  #     else
  #         POWERLINE_GIT_INFO_LEFT=" %F{white}%K{178}"$'\ue0b0'"%F{178}%F{black}%K{178}"$'$(git_prompt_info)%F{178}'
  #     fi
  #     POWERLINE_GIT_INFO_RIGHT=""
  # else
  #     POWERLINE_GIT_INFO_LEFT=""
  #     POWERLINE_GIT_INFO_RIGHT="%F{white}""%F{black}%K{white}"$'$(git_prompt_info)'" %K{white}"
  # fi

  if [ $(id -u) -eq 0 ]; then
      POWERLINE_SEC1_BG=%K{red}
      POWERLINE_SEC1_FG=%F{red}
  else
      # Color 029 is a nice bluey-green color
      POWERLINE_SEC1_BG=%K{029}
      POWERLINE_SEC1_FG=%F{029}
  fi

  POWERLINE_SEC1_TXT=%F{black}

  if [ "$POWERLINE_DETECT_SSH" != "" ]; then
    if [ -n "$SSH_CLIENT" ]; then
      POWERLINE_SEC1_BG=%K{red}
      POWERLINE_SEC1_FG=%F{red}
      POWERLINE_SEC1_TXT=%F{white}
    fi
  fi

  # PROMPT="$POWERLINE_SEC1_BG$POWERLINE_SEC1_TXT $POWERLINE_USER_NAME %k%f$POWERLINE_SEC1_FG%K{white}"$'\ue0b0'"%k%f%F{black}%K{white} "$POWERLINE_CURRENT_PATH"%F{white}"$POWERLINE_GIT_INFO_LEFT" %k""%f "

  PROMPT=$'\u2308 '$PROMPT$'\n'$'\u230A '"$ "

  if [ "$POWERLINE_DISABLE_RPROMPT" = "" ]; then
      if [ "$POWERLINE_RIGHT_A" = "" ]; then
          RPROMPT="[$POWERLINE_RIGHT_B]"
      elif [ "$POWERLINE_RIGHT_B" = "" ]; then
          RPROMPT="[$POWERLINE_RIGHT_A]"
      else
          RPROMPT="[$POWERLINE_RIGHT_B $POWERLINE_RIGHT_A]"
      fi
  fi

}

setprompt
