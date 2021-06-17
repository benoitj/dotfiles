# ~/.bashrc: executed by bash(1) for non-login shells.

set +o noclobber

# If not running interactively, don't do anything
case $- in
	*i*) ;;
	*) return ;;
esac

if ! shopt -oq posix; then
	if [ -f /usr/share/bash-completion/bash_completion ]; then
		. /usr/share/bash-completion/bash_completion
	elif [ -f /etc/bash_completion ]; then
		. /etc/bash_completion
	fi
fi

test -f ~/.config/bash/aliases && source ~/.config/bash/aliases

if [[ -d ~/.config/bash/bash_functions.d ]]; then
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
shopt -s globstar 2>/dev/null

# case insensitive globing
shopt -s nocaseglob

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
shopt -s autocd 2>/dev/null

# corret typos on tab completion
shopt -s dirspell 2>/dev/null

# correct typos on directories supplied to cd
shopt -s cdspell 2>/dev/null

# directory aliases
shopt -s cdable_vars

#### Tools configuration

# make less more friendly for non-text input files, see lesspipe(1)
#[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

PATH="/home/benoit/perl5/bin${PATH:+:${PATH}}"
export PATH
PERL5LIB="/home/benoit/perl5/lib/perl5${PERL5LIB:+:${PERL5LIB}}"
export PERL5LIB
PERL_LOCAL_LIB_ROOT="/home/benoit/perl5${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}"
export PERL_LOCAL_LIB_ROOT
PERL_MB_OPT="--install_base \"/home/benoit/perl5\""
export PERL_MB_OPT
PERL_MM_OPT="INSTALL_BASE=/home/benoit/perl5"
export PERL_MM_OPT

alias grep='grep --color=auto'
alias gpg-agent-update="gpg-connect-agent updatestartuptty /bye > /dev/null"

promptCommand() {
	local P='$'

	local r='\[\e[91m\]'
	local g='\[\e[37m\]'
	local h='\[\e[34m\]'
	local u='\[\e[95m\]'
	local p='\[\e[92m\]'
	local w='\[\e[96m\]'
	local b='\[\e[36m\]'
	local x='\[\e[0m\]'

	if test "${EUID}" == 0; then
		P='#'
		u=$r
		p=$u
	fi

	local dir
	if test "$PWD" = "$HOME"; then
		dir='~'
	else
		dir="${PWD##*/}"
		if test "${dir}" = _; then
			dir=${PWD#*${PWD%/*/_}}
			dir=${dir#/}
		elif test "${dir}" = work; then
			dir=${PWD#*${PWD%/*/work}}
			dir=${dir#/}
		fi
	fi

	local B=$(git branch --show-current 2>/dev/null)
	test "$dir" = "$B" && B='.'
	local countme="$USER@$(hostname):$dir($B)\$ "

	test -n "$B" -a -n "$(git status -s 2>/dev/null)" && b=$r
	test -n "$B" && B="$g on $b$B$g"

	local short="$u\u$g@$h\h$g:$w$dir$B $p$P$x "
	local long="$g╔ $u\u$g@$h\h$g:$w$dir$B\n$g╚ $p$P$x "
	local double="$g╔ $u\u$g@$h\h$g:$w$dir\n$g║ $B\n$g╚ $p$P$x "

	if test ${#countme} -gt "${PROMPT_MAX:-120}"; then
		PS1="$double"
	elif test ${#countme} -gt "${PROMPT_LONG:-80}"; then
		PS1="$long"
	else
		PS1="$short"
	fi
}

PROMPT_COMMAND="promptCommand"

#eval "$(starship init bash)"
