#!/bin/bash

test ! ${DISPLAY} && ${XDG_VTNR} == 1 && startx

