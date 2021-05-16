#!/bin/bash

PROFILE_D=~/.config/bash/bash_profile.d 
test -d $PROFILE_D || return 
test -f $PROFILE_D/*.sh ||  return
test -f ~/.config/bash/env && source ~/.config/bash/env

for profile_file in $PROFILE_D/*.sh
do
	source $profile_file
done

# these are fixes for discord. somehow it's missing libdrm, opt/Discord and something in mesa
# make sure you run nix-env -i mesa libdrm

export EDITOR="vim"
export BROWSER="qutebrowser"

# spaceship prompt configuration
export SPACESHIP_DIR_TRUNC_REPO=false
export SPACESHIP_GIT_BRANCH_COLOR=blue
export SPACESHIP_DIR_COLOR=green

[[ -d ~/.local/bin ]] && PATH="$HOME/.local/bin:$PATH"
[[ -d ~/.local/scripts/cli ]] && PATH="$HOME/.local/scripts/cli:$PATH"
[[ -d ~/.local/scripts/rofi ]] && PATH="$HOME/.local/scripts/rofi:$PATH"

export PATH
### ssh key agent
SSH_ENV="$HOME/.ssh/env"

function start_agent {
  echo "Initialising new SSH agent..."
  /usr/bin/ssh-agent | sed 's/^echo/#echo/' > "${SSH_ENV}"
  echo succeeded
  chmod 600 "${SSH_ENV}"
  source "${SSH_ENV}" > /dev/null
  /usr/bin/ssh-add -t 216000;
}

# Source SSH settings, if applicable

#if [ -f "${SSH_ENV}" ]; then
#  source "${SSH_ENV}" > /dev/null
  #ps ${SSH_AGENT_PID} doesn't work under cywgin
  #ps -ef | grep ${SSH_AGENT_PID} | grep ssh-agent$ > /dev/null || {
  #  start_agent;
  #}
#else
#  start_agent;
#fi

export TERMINAL=st
export _JAVA_AWT_WM_NONREPARENTING=1

#export GPG_TTY="$(tty)"
#export SSH_AUTH_SOCK="$(gpgconf --list-dirs agent-ssh-socket)"
#gpg-connect-agent updatestartuptty /bye > /dev/null 2>&1

#export KEYID=0x5425347E4304939A
#export GPG_TTY="$(tty)"
#export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
#gpgconf --launch gpg-agent
