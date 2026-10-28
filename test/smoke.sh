#!/bin/sh
# Real Pi sessions (faux provider only) in a disposable Emacs daemon,
# including a restart through `pi-deck-restart-daemon'.  Never paid calls.
set -eu

DECK_DIR=$(cd "$(dirname "$0")/.." && pwd)
PILISH_DIR=${PILISH_DIR:-$DECK_DIR/../pilish}
ORCHESTRATION_DIR=${ORCHESTRATION_DIR:-$DECK_DIR/../pi-ant/orchestration/emacs}
EMACS=${EMACS:-emacs}
PI=${PI:-$(command -v pi)}
PACKAGE_USER_DIR=${PACKAGE_USER_DIR:-$DECK_DIR/.cache/elpa/$($EMACS --batch -Q --eval '(princ emacs-major-version)')}
TMP=$(mktemp -d /tmp/pi-deck-smoke.XXXXXX)
PROJECT=$TMP/project
# An absolute server name keeps the socket private to this run.
NAME=$TMP/server
mkdir -p "$PROJECT" "$TMP/agent"
echo '{}' > "$TMP/agent/settings.json"

cleanup() {
  emacsclient -s "$NAME" --eval '(kill-emacs)' >/dev/null 2>&1 || true
  pkill -f -- "--daemon=$NAME" 2>/dev/null || true
  pkill -f -- "$TMP/agent" 2>/dev/null || true
  # Panels left open by a failed run stay in Pi's machine-wide name registry.
  for entry in "${TMPDIR:-/tmp}"/pi-orchestration-targets/*.json; do
    grep -qF "\"$NAME\"" "$entry" 2>/dev/null && rm -f "$entry"
  done
  rm -rf "$TMP"
}
trap cleanup EXIT INT TERM

cat > "$TMP/init.el" <<EOI
;;; init.el --- pi-deck smoke daemon -*- lexical-binding: t; -*-
(setq load-prefer-newer t
      package-user-dir "$PACKAGE_USER_DIR")
(package-initialize)
(dolist (dir '("$DECK_DIR" "$DECK_DIR/test" "$PILISH_DIR" "$ORCHESTRATION_DIR"))
  (add-to-list 'load-path dir))
(dolist (dir '("~/.emacs.d/tree-sitter" "~/.local/share/backups/spacemacs-20261028/tree-sitter"))
  (add-to-list 'treesit-extra-load-path (expand-file-name dir)))
(setenv "PI_CODING_AGENT_DIR" "$TMP/agent")
(setq pilish-executable '("$PI")
      pilish-extra-args '("--no-extensions" "--no-skills" "--no-prompt-templates"
                          "--no-context-files" "-e" "$DECK_DIR/test/support/faux.ts"
                          "-e" "$ORCHESTRATION_DIR/.."
                          "--provider" "pi-deck-faux" "--model" "one")
      pi-deck-state-file "$TMP/state.eld"
      pi-deck-preferences-file "$TMP/preferences.eld"
      pi-deck-restart-command '("$EMACS" "-Q" "--daemon=%s" "-l" "$TMP/init.el"))
(require 'pi-orchestration)
(require 'smoke)
(pi-deck-mode 1)
EOI

wait_for_server() {
  i=0
  until emacsclient -s "$NAME" --eval t >/dev/null 2>&1; do
    i=$((i + 1))
    [ "$i" -lt 150 ] || { echo "daemon $NAME did not start" >&2; exit 1; }
    sleep 0.2
  done
}

"$EMACS" -Q --daemon="$NAME" -l "$TMP/init.el" 2>"$TMP/daemon.log" || { cat "$TMP/daemon.log" >&2; exit 1; }
wait_for_server
RESULT=$(emacsclient -s "$NAME" --eval "(pi-deck-smoke-phase1 \"$PROJECT/\")")
echo "phase 1: $RESULT"
SESSION=$(echo "$RESULT" | sed -n 's/^"PASS1 \(.*\)"$/\1/p')
[ -n "$SESSION" ] || exit 1

# Panels need a client frame to appear in: a terminal client in a private tmux.
if command -v tmux >/dev/null 2>&1; then
  TMUX_SOCKET=pi-deck-smoke-$$
  tmux -L "$TMUX_SOCKET" -f /dev/null new-session -d -x 160 -y 50 \
    "emacsclient -s '$NAME' -t"
  i=0
  until [ "$(emacsclient -s "$NAME" --eval '(length (pi-deck--client-frames))')" -ge 1 ]; do
    i=$((i + 1)); [ "$i" -lt 100 ] || { echo "no client frame" >&2; exit 1; }
    sleep 0.1
  done
  emacsclient -s "$NAME" --eval "(pi-deck-smoke-start-panels \"$$\")" >/dev/null
  i=0
  while RESULT=$(emacsclient -s "$NAME" --eval 'pi-deck-smoke-panels-result'); [ "$RESULT" = nil ]; do
    i=$((i + 1)); [ "$i" -lt 600 ] || { echo "panel check did not finish" >&2; exit 1; }
    sleep 0.2
  done
  echo "panels: $RESULT"
  tmux -L "$TMUX_SOCKET" kill-server
  [ "$RESULT" = '"PASS-PANELS"' ] || exit 1
else
  echo "panels: skipped (no tmux)"
fi

OLD_PID=$(emacsclient -s "$NAME" --eval '(emacs-pid)')
emacsclient -s "$NAME" --eval '(run-with-timer 0.1 nil (lambda () (pi-deck-restart-daemon t)))' >/dev/null
i=0
while kill -0 "$OLD_PID" 2>/dev/null; do
  i=$((i + 1)); [ "$i" -lt 150 ] || { echo "old daemon did not exit" >&2; exit 1; }
  sleep 0.2
done
wait_for_server
NEW_PID=$(emacsclient -s "$NAME" --eval '(emacs-pid)')
[ "$NEW_PID" != "$OLD_PID" ] || { echo "daemon was not replaced" >&2; exit 1; }
echo "restarted daemon: pid $OLD_PID -> $NEW_PID"
RESULT=$(emacsclient -s "$NAME" --eval "(pi-deck-smoke-phase2 \"$SESSION\")")
echo "phase 2: $RESULT"
case "$RESULT" in '"PASS2'*) echo "smoke: PASS" ;; *) exit 1 ;; esac
