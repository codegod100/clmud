# CLMUD Development Guide

## Project Status

**Current State:** ✅ Fully Functional

- All source files have balanced parentheses
- Server compiles and runs successfully
- 1 non-fatal compilation warning exists (does not affect functionality)
- All core features working: mobs, combat, spells, equipment, vehicles

## Quick Start

### Start the Server
```bash
./dev.sh start
# or
sbcl --script mud.lisp
```

### Connect to the Server
```bash
telnet localhost 4000
# or
nc localhost 4000
```

### Run Validation
```bash
./dev.sh validate
```

## Development Tools

All development tools are located in the `tools/` directory. Use the `dev.sh` script for common tasks.

### Common Commands

```bash
# Start the server
./dev.sh start

# Validate code (balance + compilation)
./dev.sh validate

# Check compilation only
./dev.sh check

# Check parenthesis balance
./dev.sh balance

# Show running depth analysis for a specific file
./dev.sh depth src/server/commands.lisp

# List top-level forms in the command module
./dev.sh list src/server/commands.lisp

# Show a specific command form
./dev.sh show src/server/commands.lisp 46

# Clean temporary files
./dev.sh clean

# Quick connection test
./dev.sh test
```

### Tool Suite

#### Parenthesis Tools (Lisp)
- `tools/check_parens.lisp` - Enhanced balance checking with string/comment awareness
- `tools/paren-fix.lisp` - Automated parenthesis fixing with string/comment awareness

#### S-Expression Tools (SBCL)
- `tools/sexp-edit.lisp` - Manipulate complete s-expressions safely
- `tools/check-handle-command.lisp` - Validate specific functions

#### Validation Scripts (Shell)
- `tools/validate.sh` - Comprehensive validation
- `tools/check-compile.sh` - Detailed compilation check

See `tools/README.md` for detailed documentation.

## Project Structure

```
clmud/
├── src/                    # Source code
│   ├── packages.lisp       # Package definitions
│   ├── ansi.lisp          # ANSI color codes
│   ├── world.lisp         # World/room definitions
│   ├── player.lisp        # Player state and management
│   ├── inventory.lisp     # Item and inventory system
│   ├── combat.lisp        # Combat and damage mechanics
│   ├── quest.lisp         # Quest system
│   ├── mob.lisp           # Mobile entities (NPCs)
│   └── server/            # Server runtime modules (core, commands, runtime)
├── tools/                 # Development utilities
│   ├── README.md          # Tool documentation
│   ├── *.py               # Python balance checkers
│   ├── *.lisp             # SBCL manipulation tools
│   └── *.sh               # Validation scripts
├── mud.lisp              # Main entry point
├── dev.sh                # Development helper script
└── DEVELOPMENT.md        # This file
```

## Code Architecture

### Package System
- `mud.ansi` - Color output utilities
- `mud.world` - World data structures
- `mud.player` - Player management
- `mud.inventory` - Item system
- `mud.combat` - Combat mechanics
- `mud.quest` - Quest system
- `mud.mob` - NPC/mob system
- `mud.server` - Server and networking

### Key Systems

#### Command Handling
The `handle-command` function in `src/server/commands.lisp` is the main command dispatcher. It uses a large `cond` to route player commands to appropriate handlers.

#### Combat System
- Player vs Mob combat
- Spell casting (damage and healing)
- Equipment system (weapons and armor)
- Vehicle combat (ram command)
- Death and respawn mechanics

#### Mob System
- Template-based mob definitions
- Room-based spawning
- Aggressive and passive behaviors
- Loot drops and XP rewards

#### Equipment System
- Weapons and armor with stat bonuses
- Items remain in inventory when equipped
- Clear [EQUIPPED] markers in inventory display
- Stat display shows damage and armor values

#### Vehicle System
- Enter/exit vehicles
- Uber (fast travel) with uber-enabled vehicles
- Ram command for vehicle combat
- Vehicle stats (damage, speed)

## Development Workflow

### Making Changes

1. **Before editing:**
   ```bash
   ./dev.sh balance  # Verify current state
   ```

2. **Edit code** (prefer s-expression level edits)

3. **After editing - FAST DEBUGGING:**
   ```bash
   # Check parenthesis balance (FAST)
   ./dev.sh balance
   
   # Check compilation only (FAST)
   ./dev.sh check
   
   # Auto-fix any parenthesis issues (FAST)
   sbcl --script tools/paren-fix.lisp fix <file> --in-place
   ```

4. **Only when needed - FULL TESTING:**
   ```bash
   # Full validation with server start (SLOW - only when testing gameplay)
   ./dev.sh validate
   ```

### Safe Editing Practices

**DO:**
- Use `tools/sexp-edit.lisp` for structural changes
- Use `tools/paren-fix.lisp` to automatically fix parenthesis issues
- Use `./dev.sh balance` and `./dev.sh check` for fast debugging
- Test compilation before committing
- Keep temporary scripts in `tools/`
- Use fast debugging tools during development

**DON'T:**
- Manually edit parentheses - ALWAYS use `tools/paren-fix.lisp`
- Run `mud.lisp` or `sbcl --script mud.lisp` for debugging (it's too slow!)
- Run the full server during development (use `./dev.sh check` instead)
- Create one-off scripts in project root
- Ignore compilation warnings
- Edit multiple files before validating
- Use `./dev.sh validate` for every edit (it's slow)

### AI Agent Development

**For AI agents and automated development workflows, see the comprehensive guide:**
- **📋 [Project Workflow Guide](project-workflow.mdc)** - Complete AI agent instructions, debugging workflows, and testing guidelines

### Fast Debugging Tools

**⚠️ NEVER run `mud.lisp` for debugging - it's too slow!**

**Use these tools during development for quick feedback:**

```bash
# Check parenthesis balance (FAST - ~1 second)
./dev.sh balance

# Check compilation only (FAST - ~2 seconds)  
./dev.sh check

# Auto-fix parenthesis issues (FAST - ~1 second)
sbcl --script tools/paren-fix.lisp fix src/server/core.lisp --in-place

# Check specific file balance (FAST)
sbcl --script tools/check_parens.lisp check src/server/core.lisp

# Show detailed analysis (FAST)
sbcl --script tools/check_parens.lisp depth src/server/core.lisp
```

**Only use these when testing gameplay (SLOW):**
```bash
# Full validation with server start (SLOW - ~10+ seconds)
./dev.sh validate

# Start server for gameplay testing (SLOW)
./dev.sh start
```

### Debugging Workflow

**For code changes and fixes:**
1. Edit your code
2. Run `./dev.sh balance` (check parentheses)
3. Run `./dev.sh check` (check compilation)
4. If issues found, fix them and repeat
5. **DONE** - no need to start the server!

**Only start the server when:**
- Testing actual gameplay features
- Verifying player interactions work
- Testing network connectivity
- Final integration testing

**Never start the server for:**
- Syntax checking
- Compilation verification
- Parenthesis balancing
- Code structure validation

### AI Agent Workflow

**For AI agents and automated development workflows, see the comprehensive guide:**
- **📋 [Project Workflow Guide](project-workflow.mdc)** - Complete AI agent instructions, debugging workflows, and testing guidelines

### Working with S-Expressions

```bash
# List all top-level forms
sbcl --script tools/sexp-edit.lisp list src/server/commands.lisp

# Extract a form for editing
sbcl --script tools/sexp-edit.lisp show src/server/commands.lisp 46 > /tmp/form.lisp

# Edit /tmp/form.lisp manually

# Replace the form
sbcl --script tools/sexp-edit.lisp replace src/server/commands.lisp 46 \
   "$(cat /tmp/form.lisp)" src/server/commands-new.lisp

# Validate and move
./dev.sh validate src/server/commands-new.lisp && \
   mv src/server/commands-new.lisp src/server/commands.lisp
```

## Known Issues

### Compilation Warning
There is 1 non-fatal "illegal function call" compilation warning in `handle-command` (`src/server/commands.lisp`). This does not prevent the server from running and all functionality works correctly. The warning indicates SBCL detects potentially unreachable code, but it doesn't affect execution.

**Impact:** None - server runs perfectly
**Status:** Non-critical, cosmetic issue

## Testing

### Manual Testing
```bash
# Start server
./dev.sh start

# In another terminal
nc localhost 4000

# Test commands
look
n
attack goblin
cast fireball goblin
inventory
equip sword
status
help
quit
```

### Quick Test
```bash
./dev.sh test
```

### Test Management
For comprehensive testing guidelines, test file management, and AI agent testing workflows, see:
- **📋 [Project Workflow Guide](project-workflow.mdc)** - Complete testing guidelines and test file management


## Debugging Practice

**IMPORTANT**: Every time the server dies and we have to debug the trace, add a test for that specific issue to prevent regression.

### Recent Debugging Issues

1. **Talk Command Syntax Error** (2024-12-19)
   - **Issue**: Missing closing parenthesis in `talk` command caused "end of file" error
   - **Root Cause**: Incorrect nesting of `define-command`, `if`, `let*`, and `cond` forms
   - **Test Added**: `tests/unit/test-talk-command.lisp`
   - **Location**: `src/server/commands/player.lisp` lines 149-173

2. **Direction Normalization** (2024-12-19)
   - **Issue**: downstream/upstream directions causing confusion
   - **Root Cause**: Non-standard direction names
   - **Solution**: Refactored to use north/south
   - **Test Added**: TODO - Add test for direction normalization

3. **Talk Command Improvements** (2024-12-19)
   - **Issue**: Talk command failed silently when NPC not present, no abbreviation support
   - **Root Cause**: Missing mob presence check, hardcoded name matching
   - **Solution**: Added mob presence checking, abbreviation support (blackbeard/captain/black/cap)
   - **Test Added**: `tests/commands/test-talk-command-improvements.lisp`
   - **Location**: `src/server/commands/player.lisp` lines 149-186

4. **Mob Aliases System Refactor** (2024-12-19)
   - **Issue**: Hardcoded abbreviations in talk command, not extensible
   - **Root Cause**: Abbreviations were hardcoded in command logic instead of being mob properties
   - **Solution**: Added `aliases` field to mob struct, unified search system using mob IDs
   - **Test Added**: `tests/commands/test-mob-aliases.lisp`
   - **Location**: `src/mob.lisp` (mob struct + find-mob-in-room), `src/server/commands/player.lisp` (get-mob-dialogue)

5. **Mob Search System Simplification** (2024-12-19)
   - **Issue**: Aliases field was unnecessary complexity when partial name matching already worked
   - **Root Cause**: Over-engineering the search system with separate aliases field
   - **Solution**: Removed aliases field, simplified to use existing partial name matching
   - **Test Added**: `tests/commands/test-simplified-mob-search.lisp`
   - **Location**: `src/mob.lisp` (mob struct + define-mob-template + find-mob-in-room)

6. **Quest Giver System Implementation** (2024-12-19)
   - **Issue**: Quest system required commands to start quests, not immersive
   - **Root Cause**: Quest starting was command-based instead of NPC-driven
   - **Solution**: Added quest-giver field to mobs, immersive dialogue system with accept/decline
   - **Test Added**: `tests/commands/test-quest-giver-simple.lisp`
   - **Location**: `src/mob.lisp` (quest-giver field), `src/server/commands/player.lisp` (dialogue + accept/decline)

### Test Creation Guidelines

When adding tests for debugging issues:

1. **Compilation Tests**: Test that the problematic code compiles without errors
2. **Runtime Tests**: Test that the functionality works as expected
3. **Error Prevention**: Test edge cases that could cause similar issues
4. **Regression Prevention**: Ensure the specific error doesn't happen again

### Test File Locations

- Unit tests: `tests/unit/`
- Command tests: `tests/commands/`
- Integration tests: `tests/integration/`

### Running Tests

```bash
# Run all tests
./dev.sh test

# Run specific test file
sbcl --script tests/unit/test-talk-command.lisp
```

## Common Tasks

### Adding a New Command

1. Open `src/server/commands.lisp`
2. Find the `handle-command` function (form 46)
3. Add a new cond clause:
   ```lisp
   ((string= verb "mycommand")
    (write-crlf (player-stream player)
                (wrap "My command executed!" :bright-green)))
   ```
4. Validate: `./dev.sh validate`

### Adding a New Mob

1. Open `src/mob.lisp`
2. Add to `initialize-mobs`:
   ```lisp
   (define-mob :my-mob
     :name "My Creature"
     :health 50
     :damage 8
     :armor 2
     :xp-reward 20
     :aggressive t
     :loot '("health-potion"))
   ```

### Adding a New Room

1. Open `src/world.lisp`
2. Add a room definition:
   ```lisp
   (defparameter *my-room*
     (make-room :id 'my-room
                :name "My Room"
                :description "A wonderful place."
                :exits '((:north . other-room))))
   ```

## Troubleshooting

### Port Already in Use
```bash
pkill -f "sbcl.*mud.lisp"
```

### Unbalanced Parentheses
```bash
# Check balance
./dev.sh balance

# Auto-fix parentheses (RECOMMENDED)
sbcl --script tools/paren-fix.lisp fix src/server/commands.lisp --in-place

# Verify fix
./dev.sh balance
```

### Compilation Errors
```bash
./dev.sh check
less /tmp/mud-compile-detail.log
```

### Server Won't Start
```bash
sbcl --script mud.lisp 2>&1 | less
```

## Resources

- [Common Lisp HyperSpec](http://www.lispworks.com/documentation/HyperSpec/Front/)
- [SBCL Manual](http://www.sbcl.org/manual/)
- [Practical Common Lisp](http://www.gigamonkeys.com/book/)

## Contributing

1. Make changes in a focused, isolated manner
2. Validate after each change: `./dev.sh validate`
3. Test manually by connecting to the server
4. Keep tools organized in `tools/` directory
5. Document significant changes