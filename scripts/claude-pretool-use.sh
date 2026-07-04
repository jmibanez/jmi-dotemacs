#!/usr/bin/env bash
# PreToolUse(bash) guard: run all my pretool use guards

SCRIPT_PATH=$(dirname "$BASH_SOURCE")
PRETOOL_HOOKSDIR=$SCRIPT_PATH/claude-pretooluse/

for hook in $PRETOOL_HOOKSDIR/*.sh; do
    # if ! bash "$f"; then break; fi
    if ! bash "$hook"; then
        exit $?
    fi
done
