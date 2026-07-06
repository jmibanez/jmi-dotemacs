#!/usr/bin/env bash
# PreToolUse(bash) guard: run all my pretool use guards

ACTUAL_SCRIPT=$(realpath "$BASH_SOURCE")
SCRIPT_PATH=$(dirname "$ACTUAL_SCRIPT")
PRETOOL_HOOKSDIR=$SCRIPT_PATH/claude-pretooluse/

input=$(cat)
for hook in $PRETOOL_HOOKSDIR/*.sh; do
    if ! printf '%s' "$input" | bash "$hook"; then
        exit 2
    fi
done
