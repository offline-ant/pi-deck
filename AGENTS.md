# pi-deck — development guide

Emacs package: Herdr-style spaces/tabs/agent overview for Pi sessions via
Pilish. User documentation: `README.md`.

## Files

| File | Purpose |
|---|---|
| `pi-deck.el` | `pi-deck-mode`, `pi-deck-command-map`, hooks and timers |
| `pi-deck-core.el` | agent registry, state derivation, seen/done, priority, terminal prompt marks |
| `pi-deck-spaces.el` | space registry, tab helpers, agent locations, space/tab/pane commands, reveal |
| `pi-deck-workers.el` | orchestration panels and forks placed in the parent's space, closed-worker records and read-only views |
| `pi-deck-close.el` | Herdr-style closing: stop plans for tabs, panes, agents and spaces, confirmation (popup dialog from the mouse), stopping and view removal, the tab close guard |
| `pi-deck-headers.el` | pane title bars (header lines with status, model/context or exit, `[⤢]` `[✕]`) |
| `pi-deck-keys.el` | key sheet: live keymap lookups, cached column layout, clickable entries |
| `pi-deck-overview.el` | hidden root (overview) tab, `*pi-deck*` overview (ends with the key sheet), sidebar space tree (clickable rows and buttons), landing new frames on the overview |
| `pi-deck-menus.el` | popup menus: spaces, new tab, and right-click menus for agents, closed workers, spaces and tabs |
| `pi-deck-goto.el` | agent picker, jump, next attention/agent, dialog answering |
| `pi-deck-notify.el` | blocked/finished notifications |
| `pi-deck-tabbar.el` | per-space tab bar (`tab-bar-format`), its `tab-bar-map` mouse bindings, stable tab names, status cell, toggles, preferences file |
| `pi-deck-layout.el` | layout presets |
| `pi-deck-persist.el` | state file, restore, frame setup/mirroring |
| `pi-deck-daemon.el` | server info, config reload, daemon restart |
| `pi-deck-menu.el` | transient menu |
| `test/pi-deck-test.el` | ERT suite (fake Pilish sessions, real ghostel terminals/tab bar) |
| `test/smoke.sh`, `test/smoke.el`, `test/support/faux.ts` | real Pi smoke in a disposable daemon |

Dependency order: core → spaces → workers → close → headers/keys → overview → menus →
goto/notify/tabbar/layout/persist → daemon/menu → pi-deck. No cycles; forward
references use `declare-function`.

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
- A chat window plus its input window is one Pilish pair (`pilish-window-pair`)
  and one pane (`pi-deck--pane-of`). Pane code uses Pilish's pair API
  (`pilish-split-input-window`, `pilish-dissolve-window-pair`,
  `pilish-repair-window-pairs`) and never pairs windows itself; split and zoom
  panes, never their halves. `pi-deck-mode` turns on `pilish-window-pairing-mode`.
- Read Pilish sessions only through its public `pilish-session-*` accessors,
  `pilish-session-exists-p` and `pilish-view-session-file`; ask Pilish for a new
  accessor instead of reading `pilish--` state.
- Pilish never guesses pairs from restored window shapes; pi-deck re-pairs
  explicitly after `window-state-put` restores and tab switches
  (`pilish-repair-window-pairs`).
- Never split, display in or delete the selected window when it may be a side window
  (the sidebar). Commands act on `pi-deck--main-window` / `pi-deck--pane-window` (the
  most recently used chat pair, else other non-side window, when a side window is
  selected) and `pi-deck--split-pane` (splits with the pane selected, so the new window
  never shows the sidebar). `get-largest-window`/`get-mru-window` skip dedicated pair
  windows unless asked; use these helpers instead. Select a non-side window
  (`pi-deck--select-ordinary-window`) before `delete-other-windows`;
  `pi-deck--single-window` reduces a tab to one plain window.
- Only root and space tabs are saved or mirrored; tabs outside spaces stay
  frame-local.
- The tab bar renders only the current space's tabs as ordinary `tab-N` /
  `current-tab` items (non-current tabs keep a nil binding so tab-bar selects
  them); pi-deck's own items are keyed `pi-deck-*` and run on release through
  pi-deck's `tab-bar-map` bindings. Items from `pi-deck-tab-bar-extra-items`
  (keys recorded at formatting in `pi-deck--extra-item-keys`) count as own
  items, and their commands get the mouse-1 or mouse-3 click as key sequence.
- In text terminals `pi-deck--tab-bar-item` hit-tests clicks itself
  (`pi-deck--tab-bar-item-at`): `tab-bar--event-to-item` counts characters
  and the right alignment as one column, missing wide labels and every
  right-aligned item. Popup menus open on release (`mouse-3`,
  never `down-mouse-3`): a text terminal menu opened on press picks its first
  item at the release.
- Closing goes through `pi-deck--tab-close-guard`
  (`tab-bar-tab-prevent-close-functions`) and `pi-deck-close-*`; pi-deck's own
  tab closes bind `pi-deck--closing-internally` (`pi-deck--close-tabs-internally`).
  Stop confirmations put Cancel first (text terminal dialogs select the
  highlighted item on any click). Stopping removes views before killing.
- Header line buttons use `local-map` (header lines ignore `keymap`).
- The pseudo-space for agents without a space is labelled "other"; a real
  space may have that name, so test registration (`pi-deck-space-names`),
  never the label.
- Tab-bar restores can drop the pairs' atoms; `pi-deck--on-tab-select` repairs
  pairs at once so commands right after a tab switch see them.
- State saves are event-driven (`pi-deck-schedule-save`): never add
  periodic saving. Preferences changed by commands (sidebar toggle) are
  saved with `pi-deck-save-preference`.
- Key sheet entries name commands, never key strings: keys come from
  `where-is-internal` (in the overview buffer for global commands, so the
  Evil leader is found), Pilish keymaps or the transient menu. Never look
  keys up per overview refresh; the cache is reset by `after-load-functions`
  and interactive refresh.
- Orchestration targets are identified by `pi-orchestration-targets`
  (`:kind`), not by terminal major mode.
- `*.elc` files are build output (`make compile`), not tracked; the config
  loads this checkout with `load-prefer-newer`, so stale ones never shadow
  sources.

## Checks

```sh
make test     # compile with warnings as errors + ERT (emacs -Q --batch)
make smoke    # real Pi/faux in a disposable daemon, including restart
```

## Known issues

- Tabs are per frame: a new frame copies the deck, but later tab edits do not sync
  between open frames.
- Tabs created while the daemon has no client frame stay on its initial frame and do
  not reach the next client (`pi-deck--last-tabs` updates only with a client frame).
- Mirrored frames copy tabs, so closing a tab counts views in its own frame only: an
  agent shown in a mirrored frame's copy of the tab still stops.
- Evil's `SPC w r` (`evil-window-rotate-downwards`) with the sidebar open can leave
  two pairs showing the same session (Evil's side-window save/restore fails to put
  chat states into input-sized windows); the pairs stay consistent.
- Terminal status hooks `ghostel--osc133-marker` (ghostel internal); ghostel upgrades
  can break it.
