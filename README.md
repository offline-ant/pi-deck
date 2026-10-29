# pi-deck

Herdr's essentials inside Emacs, for [Pi](https://pi.dev) sessions run through
[Pilish](https://github.com/offline-ant/pilish/tree/pi-orchestration):
spaces, tabs, an agent overview with status, attention navigation,
notifications, Herdr-style closing and an overview that controls the Emacs
daemon, all usable by mouse in graphical and text frames. Built on Emacs
31's `tab-bar`, side windows, header lines and `transient`; terminals use
[ghostel](https://github.com/dakra/ghostel) (libghostty-vt).

```
 ⌂ │ ◐ pi ▾ │ ◐ main ✕ │ tests ✕ │ +                         ◉ 1 blocked · ● 1 done
 ⌂ Overview            │ ◐ main · claude-opus-5-5 · 41%                    [⤢] [✕]
                       │ …chat…
 ▾ ◐ pi             +  │ …input…
   ◐ main           ✕  │ ○ tests (exit 0)                                  [⤢] [✕]
     ◐ review       ✕  │ …terminal…
     ○ tests        ✕  │
   ▸ 2 finished workers│
 ▸ ● swarmy         +  │
                       │
 + New space           │
```

| Herdr | pi-deck |
|---|---|
| workspace | **space**: a `tab-bar` tab group with a project directory and Git branch |
| tab | **tab** in a space: a window layout |
| pane | a Pilish chat with its input below (one atomic window pair) or a ghostel terminal |
| agent | Pilish session (root or orchestration worker) or ghostel terminal |
| sidebar / goto picker | the sidebar space tree (`pi-deck-sidebar-toggle`) / `pi-deck-goto` |
| closing a tab or pane stops it | the same, unless another tab or window still shows it |
| server + clients | the Emacs daemon; every `emacsclient` frame gets the deck's tabs |

## Status

Each agent has a base state and a *seen* flag:

| Icon | Status | Meaning |
|---|---|---|
| ◉ | blocked | a Pi extension dialog (confirm, select, input, editor; e.g. the `ask` tool) awaits you |
| ● | done | finished work (working/blocked → idle) while not visible; cleared once shown |
| ◐ | working | Pilish reports activity (sending, thinking, replying, running tools, compacting); for terminals, a command runs |
| ○ | idle | waiting for input; for terminals, at the prompt or exited |
| · | unknown | Pi process gone; terminal without prompt marks |

Attention priority is blocked > done > working > idle > unknown. Tabs and
spaces show the most urgent status of their agents. Terminal status needs
prompt marks: OSC 133 (`A` prompt, `C` command, `D` done). New shell panes
run ghostel's default shell (`ghostel-shell`, else `pi-deck-shell-program`),
whose shell integration emits them for bash, zsh, fish and nushell; other
programs need their own (fish 4 natively, bash via
`pi-ant/orchestration/shell/osc133.bash`). Exited terminals keep their buffer
and show their exit status.

With `pi-deck-dialog-style` `inline` (the default), Pi dialogs never
interrupt you: the agent turns blocked and its question waits at the end of
its chat with clickable choices. Jumping to it (`RET` in the overview,
`pi-deck-goto`, `pi-deck-next-attention`) or `A` puts point on the first
choice: click, `RET` or a digit answers, `TAB` moves, `C-c C-k` cancels;
`C-u A` answers in the minibuffer instead. The input header shows
`◉ answer: …` while a question waits; clicking it goes there. A question that
follows your answer (the next `ask` question, its editor) takes focus, so
multi-question flows are answered in a row; drafts are never touched.

Notifications follow Herdr: *needs attention* on becoming blocked and
*finished* on leaving work for idle, both suppressed while the agent is
visible. They use D-Bus desktop notifications (clicking jumps to the agent)
when available, else the echo area (`pi-deck-notify-method`).

## Setup

Requires Emacs 31.1, Pilish from the `pi-orchestration` branch (pending dialog
API), ghostel 0.59.0 with its native module and, for orchestration workers,
`pi-ant/orchestration/emacs/pi-orchestration.el`.

```elisp
(use-package pi-deck
  :load-path "/home/devops/Projects/pi/pi-deck"
  :demand t
  :config
  (pi-deck-mode 1)
  (keymap-global-set "C-c d" 'pi-deck-command-map))
```

`pi-deck-mode` turns on `tab-bar-mode`, keeps a hidden overview tab (the
*root* tab group, never drawn in the tab bar) in every frame and starts
tracking agents. Run Emacs as a daemon (e.g.
`emacs --daemon=pi`) and attach with `emacsclient -t` or `-c`; each new
client frame gets the tabs of an existing frame, or of the last closed one.
A frame with nothing to copy opens on the overview: its unused initial tab
(`*scratch*` or the overview) is closed. Set
`(setq initial-buffer-choice #'pi-deck-initial-buffer)` so a frame shows the
overview even before pi-deck sets it up.

## Keys (`pi-deck-command-map`)

| Key | Command |
|---|---|
| `d` | overview ⇄ previous tab (`pi-deck-root`, also the tab bar's ⌂) |
| `b` | toggle sidebar (shown by default in every tab but the overview; the choice is saved) |
| `g` | go to agent (grouped by space; `C-u` narrows by status) |
| `n` | next agent needing attention (blocked first, then done) |
| `N` / `P` | next / previous agent in overview order |
| `a` | new Pi agent in a new tab of the current space (a tab showing no agent yet is used instead); outside a space, pick a space or **New space…** |
| `e` | new shell pane (`C-u`: split right) |
| `A` | answer the current agent's pending dialog |
| `S` / `s` | create space (project directory) / switch space |
| `R` / `K` | rename / close space (stops its agents; see [Closing](#closing)) |
| `t` / `r` / `X` | new tab (showing the space's directory) / rename / close tab (stops what only it shows) |
| `]` / `[` | next / previous tab within the space |
| `1`–`9` | tab N of the current space |
| `\|` / `-` | split pane right / below |
| `z` | zoom pane (toggle) |
| `x` | close pane (a chat with its input; stops its agent unless shown elsewhere) |
| `=` | repair the tab's panes: every chat over its input again, closed inputs reopened |
| `l` | layout preset: `single`, `chat+shell`, `grid` (all agents), `workers` |
| `m` / `?` | transient menu (also tab bar toggles, state and daemon) |

## Overview (`*pi-deck*`)

The tab bar's ⌂ (or `d`) shows it in the hidden overview tab; again returns
to the previous tab. The header shows the status summary, Emacs version,
server name, PID, uptime, clients, frames, live Pi sessions, socket,
orchestration host status, notification method and state file. Agents
follow, grouped by space (directory and branch), then by the tab showing
them ("not shown in a tab" last); live orchestration workers and panels nest
under their parent. The key sheet ends the overview (see below).

Workers are ephemeral: Pi closes them once they return, and their buffers
go. Each parent row is followed by a folded "N finished workers" line;
unfold it and press `RET` (or click) on a worker to read its saved session
in Pilish's read-only view. Parent chats link the same views: worker tool
results carry `details.savedConversations`, shown by Pilish as "Pi
sessions" links.

| Key | Action |
|---|---|
| `RET` | jump to agent (answers a pending dialog) / visit the space at point / show closed workers / read a closed worker |
| `o` | show agent beside the current pane, stay in the overview |
| `TAB` | fold or unfold the space or closed workers at point |
| `b` `w` `i` `d` `u` / `a` | filter blocked, working, idle, done, unknown / all |
| `s` | sort by spaces or by priority (then most recent change) |
| `n` `p` / `j` `k` | next / previous agent |
| `A` | answer the dialog of the agent at point |
| `K` | stop the agent at point (asks when it is working or blocked) |
| `+` `N` `E` | new space; new agent or shell in the space at point (else read) |
| `g` (`gr` under Evil) | refresh |
| `h` | show or hide the key sheet (the choice is saved) |
| `m` `?` | menu |
| `q` | back to the previous tab |

### Sidebar

A side window on the left of every tab but the overview
(`pi-deck-sidebar-side`, `pi-deck-sidebar-width`), with the same keys. It is a
space tree:

- `⌂ Overview` shows the overview.
- Each space is a header: rolled-up status, `▸`/`▾` (folds), name (visits
  the space: its most recently used tab, created when it has none; it
  unfolds), and `+` (menu: new agent, new shell, new tab). The current
  space's name is underlined.
- Under it the space's agents, workers and panels nested under their parent,
  each with status, name (bold when the current tab shows it), detail and a
  ✕ that stops it ([Closing](#closing)). Closed workers fold into "N finished
  workers" with transcript links.
- `+ New space` creates a space.

Clicking an agent jumps to it: the tab showing it is selected; a worker or
panel whose parent a tab shows opens beside its parent; any other agent
replaces the main pane of its space's current tab (`pi-deck-reveal-action`).
`mouse-2` (or `o`) shows it beside the current pane and keeps the sidebar
selected. Pane commands (`SPC d |`, `-`, `z`, `x`, layouts) run from the
sidebar act on the main view.

### Key sheet

The overview ends with a sheet of about 30 keys for agent control, grouped
as Agents, Spaces & tabs, Panes, Chat & input, Pi asks (inline dialog
keys), Windows and Daemon, in as many columns as the window fits (three at
80 columns, six at 160). Keys come from the live keymaps, so rebinding
shows up: global commands as you type them in the overview (`SPC d …` under
an Evil leader, else `C-c d …`), Pilish commands from Pilish's mode maps,
menu-only commands through the menu (`SPC d m D` restart daemon). Entries
without a key are omitted. Lookups are cached until a file loads or you
refresh (`g`/`gr`); layouts are cached per width and redone when the
window's width changes. Click an entry (or `RET` on it) to run the global
commands (new agent, go to, new space, menu, …).

`pi-deck-key-sheet` (default on, toggled by `h`) shows it;
`pi-deck-key-sheet-groups` defines the entries (label, command, and
`:maps`, `:range`, `:click`); `pi-deck-key-sheet-substitutions` shows your
own command's key in place of a standard one, e.g.
`((delete-other-windows . my/toggle-maximize-window))`.

## Mouse

Everything works by mouse in graphical and text frames (the config enables
`xterm-mouse-mode`); pi-deck's buttons and menus act on release.

- **Tab bar** (only the current space; from the overview, the last used
  space): `⌂` toggles the overview, `NAME ▾` opens a menu of spaces plus
  *New space…* and *Overview*, then the tabs (status icon, name, ✕), `+`
  (new agent, new shell or empty tab) and the status summary (click: next
  agent needing attention, else the overview). Click a tab to select it,
  drag to move it, ✕ or `mouse-2` to close it.
- **Pane title bars** (header lines of chats and terminals; Pilish's input
  windows keep their own header): status, name, model and context use (Pi)
  or exit status (terminals); `[⤢]` zooms, `[✕]` closes the pane.
- **Sidebar and overview**: see [Sidebar](#sidebar); click a space header's
  arrow to fold.
- **Right-click menus** (`mouse-3`): agent rows (Jump, Show beside, Zoom,
  Rename…, Answer question when one waits, Stop), closed workers (Open
  transcript), space headers and the space button (New agent, New shell,
  New tab, Rename…, Close space), tabs (Rename…, Close).

## Closing

Closing follows Herdr: closing a **tab** stops the agents and panels it
shows unless another tab of the frame also shows them; their workers and
panels without a view of their own stop too. Closing a **pane** (✕ in its
title bar, `x`) does the same for its agent, counting other windows of the
tab as well; the tab's only pane closes the tab. A **sidebar ✕** or `K`
stops that agent wherever it is shown. Closing a **space** stops all its
agents. Afterwards, windows showing stopped agents close, and tabs that
showed nothing else close too, in every frame. The overview tab is pinned.

Closing asks first only when something it would stop is working or blocked,
naming everything that stops. Mouse closes ask with a popup dialog (*Cancel*
/ *Stop N and close*; text terminals draw it as a menu, Cancel first so a
stray click or `RET` never stops anything); keyboard closes ask in the
minibuffer. Orchestration workers and panels stop through Pi's Emacs host
(`pi-orchestration-stop-target`), so Pi notices they are gone.

## Panels

Terminals an agent starts with `panel-start` (Pi's Emacs orchestration
host) appear at once without taking focus: below the parent agent's pane
when it is on screen, otherwise in a new tab of the parent's space
(`pi-deck-show-panels`, `pi-deck-panel-height`). Forks (`/fork-here`) get a
new background tab in the parent's space. Workers (`do`, `delegate`) never
get a tab: they are listed under their parent and open beside it when
clicked. When Pi closes a worker, fork or panel, the windows and tabs
showing only it go too.

## Sessions from other frontends

`/switch emacs` in a Pi session elsewhere (Herdr, Tau, a terminal) opens that
conversation here through `pi-deck-show-session`: a tab in the space whose
directory contains the session's cwd (created when missing), focused in the
most recently used client frame. Without a client frame, the next frame shows
it. An idle view of the same session is reloaded in place instead.

## Menu, tab bar and preferences

`pi-deck-menu` groups space, tab, agent, pane and layout commands, plus:

- tab bar toggles: tab numbers within the space, the status cell
  (`◉ 2 blocked · ● 1 done`), status icons, showing the tab bar; `W` saves
  these and sidebar/reveal preferences to
  `~/.emacs.d/var/pi-deck/preferences.eld` (never to your init files);
- state: save / restore;
- daemon: reload `user-init-file`, restart the daemon.

Tabs are named after their main agent: the first Pi agent in window order
(top left first), else the first terminal (a panel by its panel name), else
the main window's buffer. Selecting another window does not rename a tab;
explicit names (`r`, the tab menu) stay.

## Persistence and daemon restart

`pi-deck-save-state` (also about 2 s after changes — spaces, tabs, window
layouts, agents starting, closing or finishing a turn — and at exit, never
periodically; `pi-deck-save-delay`) writes
`~/.emacs.d/var/pi-deck/state.eld`: spaces, a client frame's root and space
tabs with window layouts (tabs outside spaces stay local to their frame), Pi sessions with their session files, and terminals with
directory and command. `pi-deck-restore-state` resumes Pi sessions with
`pi --session FILE` (history included), restarts terminals, and recreates
tabs of spaces the frame lacks. Orchestration workers and panels belong to
their parent Pi and are not restored. Set `pi-deck-auto-restore` to restore
whenever the mode starts.

`pi-deck-restart-daemon` lists the Pi sessions and terminals that will stop,
asks for confirmation, saves the state, starts a detached helper that waits
for this Emacs to exit and then runs `pi-deck-restart-command` (default
`emacs --daemon=<socket>`, the running server's absolute socket file, so the
replacement listens exactly where clients connect), and exits. The new daemon restores the state
when `pi-deck-mode` starts. In-flight Pi turns are interrupted; sessions
resume from disk. Reattach clients afterwards.

## Notable options

Each chat and the input window below it form one pane through Pilish's
`pilish-window-pairing-mode`, which `pi-deck-mode` turns on: ordinary
window commands (`C-x 0`, `C-x 1`, splits, `switch-to-buffer`, Evil's
window moves, `winner-undo`) keep chat and input together, and several
agents can sit side by side in one tab, each with its own input.  Pilish
sizes the inputs (`pilish-input-window-height`; `fit` grows with the draft
like Pi's editor).

`pi-deck-new-space-action` (agent/shell/dired), `pi-deck-reveal-action`
(how agents no tab shows appear: window/split/tab), `pi-deck-pane-headers`,
`pi-deck-tab-numbers`, `pi-deck-tab-bar-icons`, `pi-deck-tab-bar-status-cell`,
`pi-deck-include-shells`, `pi-deck-notify-on`,
`pi-deck-notify-shells`, `pi-deck-notify-bell`, `pi-deck-summary-statuses`,
`pi-deck-layouts` (add your own presets), `pi-deck-grid-include-shells`,
`pi-deck-sidebar`, `pi-deck-sidebar-side`, `pi-deck-sidebar-width`,
`pi-deck-key-sheet`, `pi-deck-key-sheet-groups`, `pi-deck-key-sheet-substitutions`,
`pi-deck-show-panels`, `pi-deck-panel-height`,
`pi-deck-finished-workers-limit`, `pi-deck-save-delay`, `pi-deck-status-icons`,
`pi-deck-mirror-new-frames`, `pi-deck-restart-command`,
`pi-deck-refresh-interval`.

## Development

```sh
make test     # byte-compile (warnings are errors) + ERT, emacs -Q --batch
make smoke    # real shell pane, real Pi + faux provider in a disposable daemon, incl. restart
```

Dependencies (ghostel with its prebuilt native module, Pilish's packages)
install into `.cache/elpa/<version>`;
Pilish is taken from `../pilish` (`PILISH_DIR`), orchestration from
`../pi-ant/orchestration/emacs` (`ORCHESTRATION_DIR`). The smoke test uses a
private server socket and agent directory, only the in-process faux provider
(`test/support/faux.ts`), and kills its daemons.

## Limitations

- Tabs are per frame. New frames copy the deck; later tab changes in one
  frame do not propagate to other frames.
- Status comes from this Emacs: Pi sessions started elsewhere (terminals,
  Herdr, Tau) are not shown.
- Emacs is single-threaded; many agents streaming at once share its time.
