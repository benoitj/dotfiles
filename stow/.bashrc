# ~/.bashrc: executed by bash(1) for non-login shells.

set +o noclobber

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac


if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

test -f ~/.config/bash/aliases && source ~/.config/bash/aliases

if [[ -d ~/.config/bash/bash_functions.d ]]
then
  for f in ~/.config/bash/bash_functions.d/*; do source $f; done
fi
### basic options

# prevent overwrite on redirection
# Use >| to force
set -o noclobber
set -o vi

# update window size after every command
shopt -s checkwinsize

# history expansion with space
# !!<space> replace with last command. !2<space> replace with second history
bind Space:magic-space

# Resursize globbing with **
shopt -s globstar 2> /dev/null

# case insensitive globing
shopt -s nocaseglob;

#### tab completion

# case insensitive file completion 
bind "set completion-ignore-case on"

# - and _ are the same in file completion
bind "set completion-map-case on"

# display multiple matches on first tab
bind "set show-all-if-ambiguous on"

# add trailing / on completion of dir symlink
bind "set mark-symlinked-directories on"

#### history

# append to the history file, don't overwrite it
shopt -s histappend

# save multiline commands as one
shopt -s cmdhist

# incremental history with up/down
bind '"\e[A": history-search-backward'
bind '"\e[B": history-search-forward'
bind '"\e[C": forward-char'
bind '"\e[D": backward-char'

#### directory navigation

# prepend "cd"
shopt -s autocd 2> /dev/null

# corret typos on tab completion
shopt -s dirspell 2> /dev/null

# correct typos on directories supplied to cd
shopt -s cdspell 2> /dev/null

# directory aliases
shopt -s cdable_vars

#### Tools configuration

# make less more friendly for non-text input files, see lesspipe(1)
#[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"


#export GPG_TTY="$(tty)"
#export SSH_AUTH_SOCK="$(gpgconf --list-dirs agent-ssh-socket)"
#gpg-connect-agent updatestartuptty /bye > /dev/null 2>&1

#test


#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!


alias grep='grep --color=auto'
alias gpg-agent-update="gpg-connect-agent updatestartuptty /bye > /dev/null"

#export KEYID=0x5425347E4304939A
#export GPG_TTY="$(tty)"
#export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
#gpgconf --launch gpg-agent
#
eval "$(starship init bash)"
