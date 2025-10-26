**Core Overview**
- `mud.lisp` is the SBCL entrypoint: it fixes the working directory, loads `src/packages.lisp`, then pulls in ANSI, player, inventory, merchant, world, mob, combat, quest, and server modules before calling `mud.server:start`/`await` with a port resolved from CLI/env (`--port`, `MUD_PORT`, `PORT`).
- Packages declared in `src/packages.lisp` surface module APIs; cross-module calls rely on exported symbols and early `declaim` hints (e.g. `mud.player` inlining `mud.inventory` helpers) so respect package boundaries when adding functions.
- Persistent game data is rebuilt on every boot via `initialize-world`, `initialize-merchants`, `initialize-quests`, and `initialize-mobs`, so mutations should go through those loaders rather than modifying globals directly.

**Server Runtime**
- `src/server/core.lisp` owns sockets, Telnet negotiation, ANSI filtering, and shared client state; guard `*clients*` with `*clients-lock*` and always send text with `write-crlf` to preserve CRLF expectations.
- `src/server/runtime.lisp` wraps `accept-loop` and `client-loop`; new per-connection behavior (login, repeat-last-command via ".", inventory seeding) belongs there, while room movement flows through `move-player` and `send-room-overview`.
- Shared messaging utilities (`announce-to-room`, `broadcast`) expect the mutex to be held; they also wrap names with `mud.ansi:wrap` so continue using them for cross-player chatter.

**Content & Data**
- `mud.world` stores rooms/exits; exits include gating keywords (e.g. `(:north :water . hidden-cove)`) that pair with vehicle checks, so prefer `mud.world:neighbor` instead of walking the structure yourself.
- `mud.inventory` keeps templates, slot metadata, and equips—players retain equipped items in their inventory with `[EQUIPPED]` tags, and helpers like `handle-equip-all` compare slot scores via `item-slot-score`.
- `src/merchant.lisp` registers merchants through `define-merchant`; stock entries support finite quantity, buy-back values, and summaries (`merchant-stock-summary`), while `find-merchant-in-room-by-name` normalizes punctuation/casing when matching player input.
- Corpses and XP live in `mud.combat`; `handle-get-all` bridges corpse loot into inventories and triggers quest checks through `maybe-announce-quest-rewards`.

**Command System**
- Commands are defined in `src/server/commands.lisp` via `define-command`, which both declares `command-<name>` and registers handlers in `*command-dispatch*`; avoid editing the legacy `handle-command` cond directly.
- Direction aliases (`n`, `se`, `upstream`, etc.) load once through `register-direction-command` and should continue to call `move-player` with a normalized keyword when adding shortcuts.
- Merchant (`shop`, `buy`, `sell`), looting (`get`, `get all`), and combat entries lean on helpers that already broadcast events and award quests—reuse those paths so announcements and rewards stay consistent.

**Dev Workflow**
- Prefer the wrapper scripts in `scripts/`: `./scripts/dev.sh start` boots the server, `./scripts/dev.sh test` runs the 10s smoke test via `nc`, `./scripts/dev.sh validate` executes `tools/validate.sh` (paren + compile), and `./scripts/dev.sh check` calls `tools/check-compile.sh` for faster compile-only feedback.
- Automated tests live in `tests/test-runner.lisp` (FiveAM); run with `sbcl --script tests/test-runner.lisp` once `~/quicklisp/setup.lisp` is installed.
- Structural edits should use `python3 tools/lisp-safe-edit.py` to keep parentheses balanced or `sbcl --script tools/sexp-edit.lisp` (`show`, `replace`, `check`) for S-expression-safe updates; `tools/check-handle-command.lisp` isolates the known `handle-command` warning.
- Compilation logs land in `/tmp/mud-compile-detail.log`, and `./scripts/dev.sh clean` clears temp artifacts such as `/tmp/mud-test.log` when iterating.
