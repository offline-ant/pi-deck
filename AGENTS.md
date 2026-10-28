# pi-deck — development guide

Emacs package: Herdr-style spaces/tabs/agent overview for Pi sessions via
Pilish. User documentation: `README.md`.

## Files

| File | Purpose |
|---|---|
| `pi-deck.el` | `pi-deck-mode`, `pi-deck-command-map`, hooks and timers |
| `pi-deck-core.el` | agent registry, state derivation, seen/done, priority, terminal prompt marks |
| `pi-deck-spaces.el` | space registry, tab helpers, agent locations, space/tab/pane commands, reveal |
| `pi-deck-overview.el` | root tab, `*pi-deck*` overview, sidebar |
| `pi-deck-goto.el` | agent picker, jump, next attention/agent, dialog answering |
| `pi-deck-notify.el` | blocked/finished notifications |
| `pi-deck-tabbar.el` | tab bar status cell, icons, toggles, preferences file |
| `pi-deck-layout.el` | layout presets |
| `pi-deck-persist.el` | state file, restore, frame setup/mirroring |
| `pi-deck-daemon.el` | server info, config reload, daemon restart |
| `pi-deck-menu.el` | transient menu |
| `test/pi-deck-test.el` | ERT suite (fake Pilish sessions, real EAT/tab bar) |
| `test/smoke.sh`, `test/smoke.el`, `test/support/faux.ts` | real Pi smoke in a disposable daemon |

Dependency order: core → spaces → overview → goto/notify/tabbar/layout/persist
→ daemon/menu → pi-deck. No cycles; forward references use `declare-function`.

## Rules

- Pilish integration goes through public APIs and hooks
  (`pilish-activity-phase-functions`, `pilish-extension-dialog-functions`,
  `pilish-session-pending-dialogs`, `pilish-start-session`). Needed Pilish
  changes belong in the Pilish fork (`../pilish`, branch `pi-orchestration`)
  with tests, not in workarounds here.
- Set tab groups with `pi-deck--set-tab-group`; `tab-bar-change-tab-group`'s
  default hook moves tabs.
- Never write the user's init files; preferences and state live under
  `user-emacs-directory`/var/pi-deck/.
- Tests never call paid models: ERT uses fake sessions, smoke uses Pi's faux
  provider with a private agent directory and server socket.
- Evil: the overview's keys live in its normal-state auxiliary map (built
  from `pi-deck-overview-mode-map`), which beats evil-collection's
  `special-mode` bindings; refresh is `gr` there so the `g` prefix survives.
- A chat window plus its input window is one pane (`pi-deck--pane-of`);
  split and zoom panes, never their halves.
- Only root and space tabs are saved or mirrored; tabs outside spaces stay
  frame-local.
- `*.elc` files are build output (`make compile`), not tracked; the config
  loads this checkout with `load-prefer-newer`, so stale ones never shadow
  sources.

## Checks

```sh
make test     # compile with warnings as errors + ERT (emacs -Q --batch)
make smoke    # real Pi/faux in a disposable daemon, including restart
```
