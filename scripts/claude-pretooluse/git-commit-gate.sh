#!/usr/bin/env bash
# PreToolUse hook: block `git commit` unless the sigil file
# ~/.claude/commit-authorized exists. On success, the sigil is consumed
# (one-shot) so each commit needs a fresh user authorization.
#
# Triggered on every Bash tool call; matches `git commit` as a token in
# the command string so env-prefixed (CC=... git commit ...) and
# chained (foo && git commit ...) forms are caught alongside the plain
# invocation. Non-matching commands pass through with exit 0.

set -u

PROJECT_ROOT=$(cd ~/projects/; pwd)/

input=$(cat)
command=$(printf '%s' "$input" | jq -r '.tool_input.command // ""')

# Token match for `git commit`: anchored at start or after a separator
# (space/&|;), followed by whitespace or end. Avoids matching
# "git commit-msg" (no separator after "commit") and "git-commit"
# (extension without space).
if ! printf '%s' "$command" \
    | grep -qE '(^|[[:space:]&|;])git[[:space:]]+commit([[:space:]]|$)'; then
  exit 0
fi

PROJECT_DIR="$(git rev-parse --show-toplevel)"
SIGIL="$PROJECT_DIR/.claude/commit-authorized"
PREAUTH_SEQUENCE_SIGIL="$PROJECT_DIR/.claude/commit-authorized.sequence"

# Existence of pre-auth sequence sigil means all commits are
# authorized until the pre-auth sequence sigil is manually removed
if [[ -f "$PREAUTH_SEQUENCE_SIGIL" ]]; then
  touch "$SIGIL"
fi

if [[ -f "$SIGIL" ]]; then
  rm -f "$SIGIL"
  exit 0
fi

reason="Refusing git commit: no commit-authorization sigil present. \
You must ask permission from the user, then re-issue the commit. The \
sigil is one-shot: consumed on hook execution."

jq -n --arg r "$reason" '{
  hookSpecificOutput: {
    hookEventName: "PreToolUse",
    permissionDecision: "deny",
    permissionDecisionReason: $r
  }
}'
exit 2
