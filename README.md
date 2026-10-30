# pi-deck

A sidebar of spaces and agents inside Emacs, for [Pi](https://pi.dev) sessions
run through
[Pilish](https://github.com/offline-ant/pilish/tree/pi-orchestration):
one window layout per project space, a sidebar listing every agent with its
status, workers and panels, attention navigation and notifications, all
usable by mouse in graphical and text frames. Built on Emacs 31's side
windows, header lines, (hidden) `tab-bar` tabs and `transient`; terminals use
[ghostel](https://github.com/dakra/ghostel) (libghostty-vt).

```
 ◉ 1 blocked · ● 1 done  🎤│ ◐ main · claude-opus-5-5 · 41%   [↑] [↓] [⑂] [⤢] [✕]
pi (3) ◐                +  │ …chat…
 ◐ main                 ✕  │ …input…
   ◐ review             ✕  │ ○ tests (exit 0)                             [⤢] [✕]
   ○ tests              ✕  │ …terminal…
   ▸ 2 finished workers    │
swarmy (2) ●            +  │
                           │
+ New space                │
```

- A **space** is a project directory with one window layout. Switching
  spaces switches the whole layout; there are no visible tabs.
- A **pane** is a Pilish chat with its input below (one atomic window pair)
  or a ghostel terminal, each with a title bar.
- An **agent** is a Pilish session (root, fork or orchestration worker) or a
  ghostel terminal (a shell or an agent's panel). Agents live on when no
  pane shows them; the sidebar lists them all.

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

`pi-deck-mode` starts tracking agents, shows the sidebar and sets
`tab-bar-show` to nil: each frame keeps a space's layout in a hidden tab, so
the tab bar is never drawn. Run Emacs as a daemon (e.g. `emacs --daemon=pi`)
and attach with `emacsclient -t` or `-c`; a new client frame opens the space
selected last, with its last known layout.

## Spaces

`S` (or the sidebar's `+ New space`) creates a space for a project directory
(known projects are offered; a typed existing directory is listed first,
marked `(typed)`) and shows its first pane: a Pi agent, a shell or Dired
(`pi-deck-new-space-action`). `s` or clicking a space's name switches to it.
A frame opening a space for the first time starts from the space's last
known layout, else Dired of its directory. Layouts are per frame while open;
leaving a space, closing a frame or saving remembers the layout for the next
frame that opens it.

New agents (`a`) and shells (`e`) start in the current space: the first one
takes the layout's pane, later ones split the selected pane beside it.
Outside a space they ask for a space or **New space…**.

An agent's home space is fixed: the space it was started or assigned in,
else its parent's, else the closest space whose directory contains it; only
an agent none of these place belongs to a space whose layout shows it.
Showing an agent in another space's layout never moves it.

## Sidebar

A side window (`pi-deck-sidebar-side`, default left) in every frame wide
enough for it: `pi-deck-sidebar-width` (28) columns plus
`pi-deck-sidebar-min-pane-width` (44) for panes. Narrower frames hide it;
navigate there with `g` and `n`. `b` toggles it everywhere and saves the
choice. All frames and spaces share one width; dragging the sidebar's edge
sets it everywhere and saves it.

- The **header line** shows the status summary (`◉ 2 blocked · ● 1 done`,
  the statuses in `pi-deck-summary-statuses`; just counts when it does not
  fit; "all idle" or "no agents" otherwise). Clicking it goes to the next
  agent needing attention. Buttons from `pi-deck-sidebar-header-items`
  follow at the right (see [Options](#options)).
- Each **space** is a header: name (switches to the space and unfolds it;
  on the current space, which is underlined, opens its directory in Dired
  beside the current pane), agent count (folds; `TAB` too), rolled-up
  status, and `+` (menu: new agent, new shell).
- Under it the space's **agents**, with workers, forks and panels nested
  under their parent, each with status, name, detail and a ✕ that stops it
  (see [Hiding and stopping](#hiding-and-stopping)). Rows of agents the
  frame shows are highlighted (`pi-deck-shown-agent`). Agents without a
  space come last, under "other".
- Workers are ephemeral: Pi closes them once they return, and their buffers
  go. Under each parent a folded "N finished workers" row lists them;
  unfold it and click (or `RET` on) a worker to read its saved session in
  Pilish's read-only view. Parent chats link the same views: worker tool
  results carry `details.savedConversations`, shown by Pilish as "Pi
  sessions" links.
- `+ New space` ends the list.

Positions are stable: agents are listed in creation order (new ones at the
bottom, workers and panels under their parent), and showing, selecting or
renaming an agent never reorders the list. The order is saved with the
state and restored after a daemon restart.

Clicking an agent jumps to it: an agent on screen is focused; otherwise its
space is shown, and if that does not show it, a worker or panel whose parent
is on screen opens beside the parent and any other agent replaces the most
recently used pane. Double-clicking shows it alone: its pane is zoomed, so
the other panes are hidden while the sidebar stays (`z` brings them back).
`mouse-2` (or `o`) shows it beside the current pane and keeps the sidebar
selected. Pane commands (`|`, `-`, `z`, `x`, `l`) run from the sidebar act
on the most recently used pane.

| Key | Action |
|---|---|
| `RET` | jump to agent (answers a pending dialog) / switch to the space at point (its directory when current) / show closed workers / read a closed worker |
| `o` | show agent beside the current pane, stay in the sidebar |
| `TAB` | fold or unfold the space or closed workers at point |
| `n` `p` / `j` `k` | next / previous agent |
| `A` | answer the dialog of the agent at point |
| `K` | stop the agent at point (asks when it is working or blocked) |
| `+` `N` `E` | new space; new agent or shell in the space at point (else read) |
| `g` (`gr` under Evil) | refresh |
| `m` `?` | menu |
| `q` | hide the sidebar (as `b`) |

Under Evil the sidebar stays in normal state, so leader keys, `gg` and `/`
keep working.

## Status

Each agent has a base state and a *seen* flag:

| Icon | Status | Meaning |
|---|---|---|
| ◉ | blocked | a Pi extension dialog (confirm, select, input, editor; e.g. the `ask` tool) awaits you |
| ● | done | finished work (working/blocked → idle) while not visible; cleared once shown |
| ◐ | working | Pilish reports activity (sending, thinking, replying, running tools, compacting); for terminals, a command runs |
| ○ | idle | waiting for input; for terminals, at the prompt or exited |
| · | unknown | Pi process gone; terminal without prompt marks |

Attention priority is blocked > done > working > idle > unknown. Spaces
show the most urgent status of their agents. Terminal status needs
prompt marks: OSC 133 (`A` prompt, `C` command, `D` done). New shell panes
run ghostel's default shell (`ghostel-shell`, else `pi-deck-shell-program`),
whose shell integration emits them for bash, zsh, fish and nushell; other
programs need their own (fish 4 natively, bash via
`pi-ant/orchestration/shell/osc133.bash`). Exited terminals keep their buffer
and show their exit status.

With `pi-deck-dialog-style` `inline` (the default), Pi dialogs never
interrupt you: the agent turns blocked and its question waits at the end of
its chat with clickable choices. Jumping to it (clicking it or `RET` in the sidebar,
`pi-deck-goto`, `pi-deck-next-attention`) or `A` puts point on the first
choice: click, `RET` or a digit answers, `TAB` moves, `C-c C-k` cancels.
The input header shows
`◉ answer: …` while a question waits; clicking it goes there. A question that
follows your answer (the next `ask` question, its editor) takes focus, so
multi-question flows are answered in a row; drafts are never touched.

Notifications follow Herdr: *needs attention* on becoming blocked and
*finished* on leaving work for idle, both suppressed while the agent is
visible; orchestration workers and panels never send *finished* (their
results go to the parent). They use D-Bus desktop notifications (clicking jumps to the agent)
when available, else the echo area (`pi-deck-notify-method`).
`pi-deck-notify-functions` run on the same transitions with the kind
(`blocked`/`finished`), title, body and agent, also when the method is off;
pi-sound uses it to play Herdr's sounds and show pop-ups on the machine you
sit at.

## Keys (`pi-deck-command-map`)

| Key | Command |
|---|---|
| `b` | toggle the sidebar in all frames (the choice is saved) |
| `g` | go to agent (grouped by space; `C-u` narrows by status) |
| `n` | next agent needing attention (blocked first, then done) |
| `N` / `P` | next / previous agent in sidebar order |
| `a` / `e` | new Pi agent / new shell in the current space |
| `f` | fork the Pi agent in the selected pane (as `[⑂]`) |
| `A` | answer the current agent's pending dialog |
| `S` / `s` | create / switch space |
| `R` / `K` | rename / close space (stops its agents) |
| `\|` / `-` | split pane right / below |
| `z` | zoom pane (toggle) |
| `x` | hide pane (a chat with its input; the agent keeps running) |
| `=` | repair the layout's panes: every chat over its input again, closed inputs reopened |
| `l` | grid of the space's Pi agents (`pi-deck-grid-include-shells` adds shells) |
| `m` / `?` | transient menu (also state and daemon commands) |

## Mouse

Everything works by mouse in graphical and text frames (the config enables
`xterm-mouse-mode`); pi-deck's buttons and menus act on release.

- **Sidebar**: see [Sidebar](#sidebar).
- **Pane title bars** (header lines of chats and terminals; Pilish's input
  windows keep their own header): status, name, model and context use (Pi)
  or exit status (terminals), truncated with `…` to keep the buttons visible;
  `[↑]` toggles, for that session only, whether its chat shows each final
  answer from the start, once: when the turn ends if the chat follows the
  output, else when you open or focus it, moving forward only from above
  the answer (`pilish-jump-to-answer`; dimmed when off, the default follows
  that option),
  `[↓]` jumps a Pi chat to its latest output and follows it again (as `G`),
  `[⑂]` forks a Pi session (`/fork-here`: the fork is listed in the same
  space, named `<session>-fork`; the draft is untouched, a busy agent keeps
  working), `[⤢]` zooms, `[✕]` hides the pane.
  `pi-deck-pane-header-prefix-function` can put something first, e.g. the
  window number.
- **Right-click menus** (`mouse-3`): agent rows (Jump, Show beside, Show
  alone, Rename…, Answer question when one waits; for Pi agents Fork, Stop
  turn while busy, Compact while idle, Copy session file; Stop), closed
  workers (Open transcript), space headers and their `+` (New agent, New
  shell, Rename…, Close space).

## Hiding and stopping

Closing a pane (`[✕]` in its title bar, `x`) only hides it: the agent keeps
running and stays in the sidebar. A pane that is the layout's only one
shows the space's directory instead.

Agents stop explicitly: the sidebar's ✕, `K` in the sidebar, or the agent
menu's Stop stop that agent with its workers and panels. Closing a **space**
(`K`, the space menu) stops all its agents and forgets the space. Panes
showing stopped agents go from every layout in every frame.

Stopping asks first only when something it would stop is working or
blocked, naming everything that stops. Mouse commands ask with a popup
dialog (*Cancel* / *Stop N*; text terminals draw it as a menu, Cancel first
so a stray click or `RET` never stops anything); keyboard commands ask in
the minibuffer. Orchestration workers and panels stop through Pi's Emacs
host (`pi-orchestration-stop-target`), so Pi notices they are gone.

## Panels, workers and forks

Terminals an agent starts with `panel-start` (Pi's Emacs orchestration
host) appear at once without taking focus in half of the parent agent's
pane when it is on screen: to the right when the pane is at least twice as
wide as it is high, otherwise below, as in Herdr (`pi-deck-show-panels`).
A panel whose parent is not on screen, workers (`do`, `delegate`) and forks
(`/fork-here`, in the parent's space) are only listed in the sidebar until
you open them. When Pi closes a worker, fork or panel, the panes showing it
go too.

## Sessions from other frontends

`/switch emacs` in a Pi session elsewhere (Herdr, Tau, a terminal) opens that
conversation here through `pi-deck-show-session`: in the space whose
directory contains the session's cwd (created when missing), revealed and
focused in the most recently used client frame. Without a client frame, the
next frame shows it. An idle view of the same session is reloaded in place
instead.

## Menu and preferences

`pi-deck-menu` (`m`, `?`) groups space, agent and pane commands, plus:

- state: save / restore;
- daemon: daemon info (server name, socket, PID, uptime, clients, frames,
  live Pi sessions, orchestration host, notification method, state file and
  whether this Emacs owns it, in a temporary buffer), reload
  `user-init-file`, restart the daemon.

The sidebar toggle and dragged width are saved to `~/.emacs.d/var/pi-deck/preferences.eld`
(`pi-deck-preferences-file`), never to your init files.

## Persistence and daemon restart

`pi-deck-save-state` (also about 2 s after changes — spaces, window
layouts, agents starting, closing or finishing a turn — and at exit, never
periodically; `pi-deck-save-delay`) writes
`~/.emacs.d/var/pi-deck/state.eld`: spaces, each space's layout, the current
space, Pi sessions with their session files in sidebar order, and terminals
with directory and command. `pi-deck-restore-state` resumes Pi sessions
with `pi --session FILE` (history included) and restarts terminals, starting
them `pi-deck-restore-interval` seconds apart (0.15); once all exist, client
frames show the saved current space with its layout. Orchestration workers
and panels belong to their parent Pi and are not restored; sessions you
stopped stay stopped.

The state file belongs to one Emacs server, recorded by its socket. That
server restores it whenever it starts, however it stopped (restart,
`kill-emacs`, crash, reboot), like Herdr's agent resume;
`pi-deck-auto-restore` nil turns this off (the first save then keeps the old
file as `state.eld.bak`). A daemon claims the file only once its server
runs, after the init file, so restored sessions get the Emacs orchestration
environment. Every other Emacs (no server, another daemon such as a
disposable test daemon) neither restores nor saves the file automatically;
an explicit `pi-deck-save-state` takes it over. A file recording no owner
is restored by nobody until such a save. Daemon info says when this Emacs
does not own the file.

`pi-deck-restart-daemon` lists the Pi sessions and terminals that will stop,
asks for confirmation, saves the state, starts a detached helper that waits
for this Emacs to exit and then runs `pi-deck-restart-command` (default
`emacs --daemon=<socket>`, the running server's absolute socket file, so the
replacement listens exactly where clients connect), and exits. The new
daemon restores the state like any start. In-flight Pi turns are
interrupted; sessions resume from disk. Reattach clients afterwards.

## Options

Each chat and the input window below it form one pane through Pilish's
`pilish-window-pairing-mode`, which `pi-deck-mode` turns on: ordinary
window commands (`C-x 0` on the chat, `C-x 1`, splits, `switch-to-buffer`,
`winner-undo`) keep chat and input together (`C-x 0` on the input closes
just the input), and several agents can sit side by side in one layout,
each with its own input. Pilish sizes the inputs
(`pilish-input-window-height`; `fit` grows with the draft like Pi's
editor).

`pi-deck-new-space-action` (agent/shell/dired), `pi-deck-shell-program`,
`pi-deck-pane-headers`, `pi-deck-include-shells`, `pi-deck-dialog-style`,
`pi-deck-answer-on-jump`, `pi-deck-notify-method`, `pi-deck-notify-on`,
`pi-deck-notify-shells`, `pi-deck-notify-bell`, `pi-deck-summary-statuses`,
`pi-deck-grid-include-shells`, `pi-deck-sidebar`, `pi-deck-sidebar-side`,
`pi-deck-sidebar-width`, `pi-deck-sidebar-min-pane-width`,
`pi-deck-sidebar-header-items`,
`pi-deck-show-panels`, `pi-deck-finished-workers-limit`,
`pi-deck-status-icons`, `pi-deck-save-delay`, `pi-deck-state-file`,
`pi-deck-preferences-file`, `pi-deck-restart-command`,
`pi-deck-auto-restore`, `pi-deck-restore-interval`,
`pi-deck-refresh-interval`.

Other packages add buttons at the right of the sidebar's header line
through `pi-deck-sidebar-header-items`: functions called without arguments
returning a string or nil. Header lines ignore `keymap`, so a button binds
its clicks with a `local-map` text property, e.g. `<header-line> <mouse-1>`
to a command reading `(interactive "e")`. pi-sound's dictation button 🎤 is
one.

## Development

```sh
make test     # byte-compile (warnings are errors) + ERT, emacs -Q --batch
make smoke    # real shell pane, real Pi + faux provider in a disposable daemon, incl. restart and kill+start restore
```

Dependencies (ghostel with its prebuilt native module, Pilish's packages)
install into `.cache/elpa/<version>`;
Pilish is taken from `../pilish` (`PILISH_DIR`), orchestration from
`../pi-ant/orchestration/emacs` (`ORCHESTRATION_DIR`). The smoke test uses a
private server socket and agent directory, only the in-process faux provider
(`test/support/faux.ts`), and kills its daemons.

## Limitations

- Layouts are per frame while open: two frames showing the same space have
  separate layouts, and a frame opening a space starts from the layout
  remembered last.
- Status comes from this Emacs: Pi sessions started elsewhere (terminals,
  Herdr, Tau) are not shown.
- Emacs is single-threaded; many agents streaming at once share its time.
