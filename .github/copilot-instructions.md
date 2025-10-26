**Architecture Map**
- `mud.lisp` is the SBCL entrypoint: it sets default paths, loads `src/packages.lisp`, and then pulls in the ANSI, world, player, inventory, mob, combat, quest, and server modules before invoking `mud.server:start`/`await`.
- Packages in `src/packages.lisp` expose module APIs; cross-module calls are explicit (e.g., `mud.inventory` functions are aliased in `mud.player` via early `declaim` forms for inlining).
- `mud.server` manages sockets with SBCL threads (`make-thread`) and guards shared state (`*clients*`) with `*clients-lock*`; telnet negotiation happens in `read-telnet-line`/`handle-iac` and all outbound text must use `write-crlf` to append CRLF like a MUD client expects.
- Server startup (`start`) initializes world, quest, and mob data every time; exiting calls `stop`, which tears down sockets, joins the accept loop, and closes each client stream.
- Port selection flows through `mud.lisp` helpers (`detect-port`), so tests or scripts can override via `--port`, `MUD_PORT`, or `PORT` env vars.

**Command Lifecycle**
- Commands in `src/server/commands.lisp` are registered via the `define-command` macro, which defines `command-<name>` functions and records handlers in `*command-dispatch*`; `handle-command` simply looks up the lowercased verb and invokes the handler.
- Prefer adding clauses with `define-command` instead of editing the giant `cond` in `handle-command`; the documentation refers to "form 46" but the macro is now the authoritative extension point.
- Movement aliases (`n`, `se`, `upstream`, etc.) are wired at load time by `register-direction-command`, which resolves to `move-player`; mimic that flow if you introduce new shortcuts.
- Player-visible output is consistently run through `mud.ansi:wrap` for color and styling; keep ANSI usage centralized there and call `announce-to-room`/`broadcast` to reach other players while holding the client mutex when needed.
- Combat helpers like `handle-cast` and `command-attack` bridge to `mud.combat` and `mud.mob`; follow the existing pattern of computing damage, applying armor, and triggering loot/XP via those modules.

**Content Systems**
- `mud.world` (`initialize-world`) seeds rooms, vehicles, and facets; exits are stored as lists where a keyword in the cdr (e.g., `(:north :water . hidden-cove)`) enforces vehicle gating—use `mud.world:neighbor` to respect those rules.
- `mud.inventory` maintains item templates and equips via slots (`:weapon`, `:armor`); inventory listings annotate equipped items with `[EQUIPPED]` while retaining them in the backpack, so armor/weapon swaps never drop items.
- `mud.mob` seeds mobs from templates into `*room-mobs*`; respawns or custom encounters should operate through `spawn-mob` and `remove-mob-from-room` so the shared hash tables stay consistent.
- `mud.combat` owns spell definitions, corpse creation, and XP payout side effects; when players die, `handle-player-death` teleports them and caches their inventory in `*corpse-data*` for later looting.
- `mud.quest` tracks per-player quest state in a hash table stored on the player object; new quests should follow the `define-quest` signature (id, name, description, completion lambda, reward XP/text).

**Dev Workflow**
- Use `scripts/dev.sh start` to boot the server (wraps `sbcl --script mud.lisp`) and `scripts/dev.sh test` for a 10-second smoke test that pipes `quit` via `nc`.
- `scripts/dev.sh validate` delegates to `tools/validate.sh`, running both parentheses balance and compilation checks; `scripts/dev.sh check` hits only `tools/check-compile.sh` if you want faster feedback.
- Editing Lisp directly is risky—prefer `tools/sexp-edit.lisp` for structural operations (`list`, `show`, `replace`) or `tools/lisp-safe-edit.py` for line edits that keep depth balanced.
- Automated tests live in `tests/test-runner.lisp` and depend on Quicklisp + `fiveam`; run them with `sbcl --script tests/test-runner.lisp` after ensuring `~/quicklisp/setup.lisp` exists.
- Detailed compile logs land in `/tmp/mud-compile-detail.log` via the tooling; call `scripts/dev.sh clean` to clear temp artifacts like `/tmp/mud-test.log` when needed.


**Environment Setup**
- if shell is fish, be sure to take that into account when issuing shell commands
