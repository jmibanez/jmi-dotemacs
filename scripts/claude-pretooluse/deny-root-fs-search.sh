#!/usr/bin/env bash
# PreToolUse (Bash) guard: deny filesystem-walking tools started at root "/".
# Payload arrives as JSON on stdin; on a match we emit a deny decision.

cmd="$(jq -r '.tool_input.command // empty' 2>/dev/null)"
[ -n "$cmd" ] || exit 0     # nothing to inspect / jq missing -> stay out of the way

# A risky tool invoked as a word (also matches /usr/bin/find, not "refind"/"grepped")
risky='(^|[^[:alnum:]_])(find|grep|egrep|fgrep|rg|ripgrep|fd|fdfind|ag)([^[:alnum:]_]|$)'
# A bare, UNQUOTED "/" argument: whitespace/start before, whitespace/end/;|&) after
root='(^|[[:space:]])/([[:space:];|&)]|$)'

if printf '%s' "$cmd" | grep -Eq "$risky" && printf '%s' "$cmd" | grep -Eq "$root"; then
    jq -n --arg r "Blocked by no-root-search guardrail: filesystem walk rooted at '/'. Narrow to a specific directory (project dir, ~/.cargo/registry/src, the nearest known parent)." \
       '{hookSpecificOutput:{hookEventName:"PreToolUse",permissionDecision:"deny",permissionDecisionReason:$r}}'
    exit 1
fi
exit 0
