# pi-deck

Herdr's essentials inside Emacs, for [Pi](https://pi.dev) sessions run through
[Pilish](https://github.com/offline-ant/pilish/tree/pi-orchestration):
spaces, tabs, an agent overview with status, attention navigation,
notifications, and a root tab that controls the Emacs daemon. Built on Emacs
31's `tab-bar`, side windows and `transient`; terminals use
[ghostel](https://github.com/dakra/ghostel) (libghostty-vt).

| Herdr | pi-deck |
|---|---|
| workspace | **space**: a `tab-bar` tab group with a project directory and Git branch |
| tab | **tab** in a space: a window layout |
| pane | window showing a Pilish chat (with its input below) or a ghostel terminal |
| agent | Pilish session (root or orchestration worker) or ghostel terminal |
| sidebar / goto picker | `pi-deck-sidebar-toggle` / `pi-deck-goto` |
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

`pi-deck-mode` turns on `tab-bar-mode`, pins a **root** tab first in every
frame and starts tracking agents. Run Emacs as a daemon (e.g.
`emacs --daemon=pi`) and attach with `emacsclient -t` or `-c`; each new
client frame gets the tabs of an existing frame, or of the last closed one.

## Keys (`pi-deck-command-map`)

| Key | Command |
|---|---|
| `d` | root overview ⇄ previous tab (`pi-deck-root`) |
| `b` | toggle sidebar (shown by default in every tab but the root; the choice is saved) |
| `g` | go to agent (grouped by space; `C-u` narrows by status) |
| `n` | next agent needing attention (blocked first, then done) |
| `N` / `P` | next / previous agent in overview order |
| `a` | new Pi agent in the current space (`pilish-start-session` in its directory); outside a space, pick a space or **New space…** |
| `e` | new shell pane (`C-u`: split right) |
| `A` | answer the current agent's pending dialog |
| `S` / `s` | create space (project directory) / switch space |
| `R` / `K` | rename / close space (`C-u K` also stops its agents) |
| `t` / `r` / `X` | new / rename / close tab (the root tab is pinned) |
| `]` / `[` | next / previous tab within the space |
| `1`–`9` | tab N of the current space |
| `\|` / `-` | split pane right / below |
| `z` | zoom pane (toggle) |
| `x` | close pane (a chat pane takes its input window along) |
| `l` | layout preset: `single`, `chat+shell`, `grid` (all agents), `workers` |
| `m` / `?` | transient menu (also tab bar toggles, state and daemon) |

## Root overview (`*pi-deck*`)

The header shows the status summary, Emacs version, server name, PID,
uptime, clients, frames, live Pi sessions, socket, orchestration host status,
notification method and state file. Agents follow, grouped by space
(directory and branch), then by the tab showing them ("not shown in a tab"
last); live orchestration workers and panels nest under their parent.

Workers are ephemeral: Pi closes them once they return, and their buffers
go. Each parent row is followed by a folded "N finished workers" line;
unfold it and press `RET` (or click) on a worker to read its saved session
in Pilish's read-only view. Parent chats link the same views: worker tool
results carry `details.savedConversations`, shown by Pilish as "Pi
sessions" links. The sidebar lists live agents only.

| Key | Action |
|---|---|
| `RET` | jump to agent (answers a pending dialog) / fold space / show closed workers / read a closed worker |
| `o` | show agent in another window, stay in the overview |
| `TAB` | fold or unfold the space or closed workers at point |
| `b` `w` `i` `d` `u` / `a` | filter blocked, working, idle, done, unknown / all |
| `s` | sort by spaces or by priority (then most recent change) |
| `n` `p` / `j` `k` | next / previous agent |
| `A` | answer the dialog of the agent at point |
| `K` | stop the agent at point |
| `+` `N` `E` | new space; new agent or shell in the space at point (else read) |
| `g` (`gr` under Evil) | refresh |
| `m` `?` | menu |
| `q` | back to the previous tab |

The sidebar shows the same data compactly and uses the same keys.

## Mouse

In graphical and text frames (the config enables `xterm-mouse-mode`):
click a tab to select it, a space (group) name to visit it, `mouse-2` or the
tab's `x` to close it, `mouse-3` for its menu; click the status cell to jump
to the next agent needing attention (else the overview). In the overview and
sidebar, click a row to jump to its agent, `mouse-2` shows it in another
window, and clicking a space header (or a closed-workers line) folds it.

## Panels

Terminals an agent starts with `panel-start` (Pi's Emacs orchestration
host) appear at once without taking focus: below the parent agent's pane
when it is on screen, otherwise in a new tab of the parent's space
(`pi-deck-show-panels`, `pi-deck-panel-height`). When the panel closes,
the window or tab opened for it goes too. Workers stay hidden until you
jump to them.

## Menu, tab bar and preferences

`pi-deck-menu` groups space, tab, agent, pane and layout commands, plus:

- tab bar toggles: grouped tabs, other spaces' tabs, tab numbers, the status
  cell (`◉ 2 blocked · ● 1 done`), status icons, showing the tab bar; `W`
  saves these and sidebar/reveal preferences to
  `~/.emacs.d/var/pi-deck/preferences.eld` (never to your init files);
- state: save / restore;
- daemon: reload `user-init-file`, restart the daemon.

Tabs are named after the agent in their selected window (`pi:NAME`, or the
terminal's name) unless renamed explicitly.

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

`pi-deck-new-space-action` (agent/shell/dired), `pi-deck-reveal-action`
(how hidden agents such as workers are shown: split/window/tab),
`pi-deck-input-height`, `pi-deck-include-shells`, `pi-deck-notify-on`,
`pi-deck-notify-shells`, `pi-deck-notify-bell`, `pi-deck-summary-statuses`,
`pi-deck-layouts` (add your own presets), `pi-deck-grid-include-shells`,
`pi-deck-sidebar`, `pi-deck-sidebar-side`, `pi-deck-sidebar-width`,
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
