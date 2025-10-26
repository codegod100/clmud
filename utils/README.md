# MUD Utilities

This directory contains utility scripts for development and debugging of the Common Lisp MUD.

## Available Utilities

### quick.lisp
Quick setup utility for loading Quicklisp and FiveAM testing framework.

**Usage:**
```bash
sbcl --script utils/quick.lisp
```

**Features:**
- Loads Quicklisp setup
- Quickloads FiveAM testing framework
- Provides quick access to testing dependencies

### repl.lisp
Interactive REPL environment for debugging and testing MUD components.

**Usage:**
```bash
sbcl --script utils/repl.lisp
```

**Features:**
- Loads all MUD dependencies and packages
- Initializes the world for testing
- Provides helper functions for debugging:
  - `(show-rooms)` - List all defined rooms
  - `(show-sky-rooms)` - List sky rooms and exits
  - `(show-room 'id)` - Inspect specific room
  - `(make-test-player)` - Create a test player
  - `(visualize-sky-map)` - Show connected sky rooms
  - `(draw-sky-map)` - Draw ASCII map of sky rooms

**Available packages:**
- `mud.world` - World and room management
- `mud.player` - Player management
- `mud.mob` - Mob system
- `mud.combat` - Combat system
- `mud.quest` - Quest system
- `mud.server` - Server core

**Example session:**
```lisp
CL-USER> (show-sky-rooms)
Sky rooms:
  SKY-PLATFORM-CENTRAL: Central Sky Platform
    Exits: ((:NORTH . SKY-PLATFORM-NORTH) (:SOUTH . SKY-PLATFORM-SOUTH))
  ...

CL-USER> (make-test-player :name "DebugPlayer")
Created player DebugPlayer in room SKY-PLATFORM-CENTRAL
#<PLAYER {1001234567}>

CL-USER> (draw-sky-map)
Sky Map (centered on SKY-OVER-VILLAGE):
[SKY-OVER-VILLAGE] [SKY-PLATFORM-CENTRAL]
     ...
```

## Usage Guidelines

1. **For quick testing setup:** Use `quick.lisp` when you need FiveAM or other testing dependencies
2. **For interactive debugging:** Use `repl.lisp` when you need to explore the MUD world, test functions, or debug issues
3. **Keep utilities focused:** Each utility should have a specific purpose
4. **Document new utilities:** Add documentation here when adding new utility scripts

## Integration with Development Workflow

These utilities complement the tools in the `tools/` directory:
- `tools/` contains file manipulation and validation tools
- `utils/` contains runtime utilities and debugging aids
- Both directories help maintain a clean project root
