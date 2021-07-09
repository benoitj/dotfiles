#!/usr/bin/env bash
set -euo pipefail

echo "batch tagging new mails"
notmuch tag --batch --input=$HOME/.config/notmuch/notmuch-batch.tags
