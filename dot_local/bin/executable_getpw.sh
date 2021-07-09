#!/usr/bin/env bash

keepassxc-cli show -a Password $HOME/shared-passwords/MyPasswords.kdbx $*
