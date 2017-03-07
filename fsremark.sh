#!/bin/bash

# Shell script provided for convenience (mostly for avoiding 'mono').
# HOW-TO:
# 1) Paste this script into the system path environment.
# 2) Mark it as executable (`sudo chmod +x fsremark`)
# 3) Replace "</path/to/>" with the actual location of the remarks project
# 4) Use freely, at your convenience.
#
# Remember to be nice to your students when you grade their reports! :)


program="</path/to/>FSRemark/main.exe"

mono "$program" $1
