#!/bin/sh

if test "$1" == "-p"
then
    OPTS=shift
fi

file="$1"

test -f "$file" || exit 1

open_by_ext() {

    set -f
    case "$file" in
        *.png|*.jpg|*.jpeg|*.mkv|*.m4v|*.flac|*.mp3|*.mp4|*.ogg) mediainfo "$file";;
        *.md) glow $OPTS -s dark "$file";;
        *.pdf) pdftotext "$file" -;;
        *.ods) ssconvert -T Gnumeric_html:html40 ~/Documents/mining.ods fd://1 | lynx -dump -stdin -width 120;;
        *.html|*.xhtml|*.xml) w3m -dump -cols 120 "$file";;
        *.zip|*.jar) zipinfo "$file";;
        *.tar.gz|*.tgz) tar -ztvf "$file";;
        *.tar.bz2) tar -jtvf "$file";;
        *.tar) tar -tvf "$file";;
        *.rar) unrar l "$file";;
        *.7z) 7z l "$file";;
        *.sh|*.zsh*|*.org) pistol "$file";;
        *) highlight -O ansi --force "$file";;
    esac

}

open_by_ext
