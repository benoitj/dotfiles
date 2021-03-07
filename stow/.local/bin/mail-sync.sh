#!/usr/bin/env bash
set -euo pipefail

echo "Moving messages according to notmuch tags"

# Make sure we have updated paths
notmuch new --no-hooks

# Archive files that are NOT tagged Inbox
#filter="folder:company/INBOX -tag:inbox"
#echo "`notmuch count $filter` archived messages"
#notmuch search --output=files --format=text0 $filter | xargs -0 --no-run-if-empty mv -t ~/mail/company/Archive/new/

#filter="tag:deleted tag:spam tag:junk AND (folder:/company/INBOX/ OR folder:/company/Archive/ AND NOT folder:/Junk/)"
#echo "`notmuch count $filter` junk messages"
#notmuch search --output=files --format=text0 $filter | xargs -0 --no-run-if-empty mv -t ~/mail/company/Junk\ E-Mail/new/

# Make sure we have updated paths
notmuch new --no-hooks


[[ $# -eq 0 ]] && sync="-a" || sync="$1"

mbsync $sync

notmuch new
