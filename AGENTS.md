# pi-deck — development guide

Emacs package: project spaces (one window layout each) and an agent sidebar
for Pi sessions via Pilish. User documentation: `README.md`.

## Files

| File | Purpose |
|---|---|
| `pi-deck.el` | `pi-deck-mode` (hides the tab bar), `pi-deck-command-map`, hooks and timers |
| `pi-deck-core.el` | agent registry, state derivation, seen/done, priority, terminal prompt marks |
| `pi-deck-spaces.el` | space registry, hidden per-space layout tabs and remembered layouts (`pi-deck--layouts`, `pi-deck--last-space`), agent locations, space/pane commands, reveal |
| `pi-deck-workers.el` | orchestration panels beside their parent, forks assigned to the parent's space, closed-worker records and read-only views |
| `pi-deck-close.el` | hiding panes, stopping agents (with descendants) and closing spaces, confirmation (popup dialog from the mouse), view removal across frames and layouts |
| `pi-deck-headers.el` | pane title bars (header lines with optional prefix, status, model/context or exit, `[↑]` `[↓]` `[⑂]` (Pi) `[⤢]` `[✕]`) |
| `pi-deck-sidebar.el` | sidebar space tree (clickable rows and buttons), its header line (status summary, `pi-deck-sidebar-header-items`), shared width and narrow-frame hiding, preferences file |
| `pi-deck-menus.el` | popup menus: right-click menus for agents, closed workers and spaces; forking (`pi-deck-fork`) |
| `pi-deck-goto.el` | agent picker, jump, show alone, next attention/agent, dialog answering |
| `pi-deck-notify.el` | blocked/finished notifications; `pi-deck-notify-functions` hook for other deliveries (pi-sound) |
| `pi-deck-layout.el` | grid of the space's agents |
| `pi-deck-persist.el` | state file (version 2), restore, frame setup, `pi-deck-show-session` |
| `pi-deck-daemon.el` | server info (`pi-deck-daemon-info`), config reload, daemon restart |
| `pi-deck-menu.el` | transient menu |
| `test/pi-deck-test.el` | ERT suite (fake Pilish sessions, real ghostel terminals) |
| `test/smoke.sh`, `test/smoke.el`, `test/support/faux.ts` | real Pi smoke in a disposable daemon |

Dependency order: core → spaces → workers → close → headers → sidebar → menus →
goto/notify/layout/persist → daemon/menu → pi-deck. No cycles; forward
references use `declare-function`.

## Rules

- Pilish integration goes through public APIs and hooks
  (`pilish-activity-phase-functions`, `pilish-extension-dialog-functions`,
  `pilish-session-pending-dialogs`, `pilish-start-session`). Needed Pilish
  changes belong in the Pilish fork (`../pilish`, branch `pi-orchestration`)
  with tests, not in workarounds here.
- `pi-deck-agents` returns agents in creation order (`pi-deck-agent-number`);
  keep it the one stable order (saved state lists agents in it and restore
  re-tracks them in it). `pi-deck-agent-space` is the fixed home space
  (assigned, parent, directory, displaying layout only as a last resort); use
  `pi-deck--agent-tab` when you need which layout displays an agent.
- A space's layout in a frame is one hidden `tab-bar` tab whose group is the
  space name; `tab-bar-show` stays nil while `pi-deck-mode` is on. Switch
  spaces only with `pi-deck-select-space` (it remembers the layout being
  left); set tab groups with `pi-deck--set-tab-group`
  (`tab-bar-change-tab-group`'s default hook moves tabs). Remembered layouts
  are `window-state-get` of `window-main-window`, so side windows (the
  sidebar) are never part of them.
- Never write the user's init files; preferences and state live under
  `user-emacs-directory`/var/pi-deck/.
- Tests never call paid models: ERT uses fake sessions, smoke uses Pi's faux
  provider with a private agent directory and server socket.
- Evil: the sidebar's keys live in its normal-state auxiliary map (built
  from `pi-deck-sidebar-mode-map`), which beats evil-collection's
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
  explicitly after `window-state-put` restores and space switches
  (`pilish-repair-window-pairs`).
- Never split, display in or delete the selected window when it may be a side window
  (the sidebar). Commands act on `pi-deck--main-window` / `pi-deck--pane-window` (the
  most recently used chat pair, else other non-side window, when a side window is
  selected) and `pi-deck--split-pane` (splits with the pane selected, so the new window
  never shows the sidebar). `get-largest-window`/`get-mru-window` skip dedicated pair
  windows unless asked; use these helpers instead. Select a non-side window
  (`pi-deck--select-ordinary-window`) before `delete-other-windows`;
  `pi-deck--single-window` reduces a layout to one plain window.
- The sidebar has one width for all frames and spaces: `pi-deck--sidebar-sync`
  enforces `pi-deck-sidebar-width`, and `pi-deck--on-window-size-change` adopts
  a dragged width (a sidebar resized within an unchanged frame). Frames
  narrower than twice the width hide it.
- Popup menus open on release (`mouse-3`, never `down-mouse-3`): a text
  terminal menu opened on press picks its first item at the release.
- Closing a pane only hides it (`pi-deck-hide-pane`); agents stop only through
  `pi-deck-stop`/`pi-deck-stop-agents` and `pi-deck-close-space`. Stop
  confirmations put Cancel first (text terminal dialogs select the
  highlighted item on any click). Stopping removes views before killing.
- Header line buttons use `local-map` (header lines ignore `keymap`).
- The pseudo-space for agents without a space is labelled "other"; a real
  space may have that name, so test registration (`pi-deck-space-names`),
  never the label.
- Tab switches (space switches) can drop the pairs' atoms;
  `pi-deck--on-tab-select` repairs pairs at once so commands right after a
  space switch see them.
- State saves are event-driven (`pi-deck-schedule-save`): never add
  periodic saving. Preferences changed by commands (sidebar toggle) are
  saved with `pi-deck-save-preference`.
- The state file has one owner, the server socket it records. Automatic
  saves (`pi-deck--save-now`, also used by restart and `kill-emacs-hook`) need
  `pi-deck--state-owner`, and none run during a staggered restore
  (`pi-deck--restoring`). `pi-deck-claim-state` decides ownership and
  restores once per Emacs, in a daemon only from `emacs-startup-hook` (the
  server starts after the init file; sessions spawned earlier miss the
  orchestration environment). Tests bind a fake owner in
  `pi-deck-test--with-deck`; disposable daemons never own the user's file.
- Orchestration targets are identified by `pi-orchestration-targets`
  (`:kind`), not by terminal major mode.
- `*.elc` files are build output (`make compile`), not tracked; the config
  loads this checkout with `load-prefer-newer`, so stale ones never shadow
  sources.

## Checks

```sh
make test     # compile with warnings as errors + ERT (emacs -Q --batch)
make smoke    # real Pi/faux in a disposable daemon: restart, then kill-emacs + fresh start
```

## Known issues

- Layouts are per frame while open: two frames showing one space keep separate
  layouts, and `pi-deck--layouts` keeps whichever was remembered last.
- Evil's `SPC w r` (`evil-window-rotate-downwards`) with the sidebar open can leave
  two pairs showing the same session (Evil's side-window save/restore fails to put
  chat states into input-sized windows); the pairs stay consistent.
- Terminal status hooks `ghostel--osc133-marker` (ghostel internal); ghostel upgrades
  can break it.
