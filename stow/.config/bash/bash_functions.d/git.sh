#!/bin/bash

GIT_SSH="ssh homer"
REPOS_ROOT="src/repos/"

gls() {

	$GIT_SSH "find $REPOS_ROOT -type d -name '*.git' -prune 2>/dev/null" | while read gitdir
	do
		echo "ssh://homer.benoitj.ca/"$(echo $gitdir | sed -e "s|.*$REPOS_ROOT|~/$REPOS_ROOT|")
	done
} && export -f gls

gmk() {

	REPO_ROOT="~/$REPOS_ROOT$(echo $1 | sed -e 's/.git$//')".git

	gls | grep -q "$REPO_ROOT"
	if test $? == 0
	then
		echo "The repository $REPO_ROOT already exists"
		return 1
	fi

	echo "creating repository $REPO_ROOT"
	$GIT_SSH "git init --bare $REPO_ROOT"

	echo "git clone ssh://homer.benoitj.ca/$REPO_ROOT"
} && export -f gmk


grm() {

	REPO_ROOT="~/$REPOS_ROOT$(echo $1 | sed -e 's/.git$//')".git

	gls | grep -q "$REPO_ROOT"
	if test $? != 0
	then
		echo "The repository $REPO_ROOT does not exists"
		return 1
	fi

	echo "Removing repository $REPO_ROOT"
	CMD="$GIT_SSH rm -rf $REPO_ROOT"
	echo "Running the command '$CMD'?"
	read value

	test "$value" == "y" && $CMD
	
} && export -f gmk
