# Quest and Leveling System

## Overview

The MUD now features a complete XP/leveling system and quest framework. Players can gain experience points (XP) by completing quests, which allows them to level up and grow stronger.

## Leveling System

### XP Requirements
- Each level requires: `level * 100` XP
  - Level 1 → 2: 200 XP
  - Level 2 → 3: 300 XP
  - Level 3 → 4: 400 XP
  - etc.

### Level Up Benefits
When a player levels up, they receive:
- **+10 Max Health** (fully healed)
- **+5 Max Mana** (fully restored)

### Commands

**`status`** - View your current stats:
```
Hero - Level 2
Health: 110/110  Mana: 55/55
XP: 200/300  (Need 100 more to level)
```

## Quest System

### Available Quests

#### The First Harvest
- **How to start:** `quest start apple`
- **Objective:** Pick up an apple from the village garden
- **Location:** South of Village Square
- **Reward:** 200 XP (enough to reach level 2!)
- **Description:** The village elder needs a fresh apple from the garden.

### Quest Commands

**`quest`** - View your active quests

**`quest start apple`** - Start the apple picking quest

### Quest Completion

Quests are completed automatically when their conditions are met. For example, the apple quest completes immediately when you pick up the apple:

```
> get apple
You get apple.
Quest Complete: The First Harvest
The elder smiles warmly. 'Thank you, young adventurer. Your journey has just begun!'
You gained 200 XP!
*** LEVEL UP! You are now level 2! ***
Health: +10 (now 110)  Mana: +5 (now 55)
```

## Quest States

Each quest can be in one of three states:
- **Not Started** - Quest is available but player hasn't started it
- **In Progress** - Player has started the quest but hasn't completed it
- **Completed** - Quest is finished and rewards have been claimed

## Implementation Details

### Files
- `src/quest.lisp` - Quest system implementation
- `src/player.lisp` - XP and leveling logic
- Quest IDs use keywords (`:apple-picking`) for package independence

### Adding New Quests

To add a new quest, edit `src/quest.lisp` in the `initialize-quests` function:

```lisp
(define-quest :my-quest-id
              "Quest Name"
              "Quest description shown to player"
              (lambda (player)
                ;; Return T when quest is complete
                (has-item-in-inventory-p player "special-item"))
              500  ; XP reward
              "Completion message shown to player")
```

Then add the quest start command in `src/server.lisp` in the quest handler.

## Future Enhancements

Potential improvements:
- Multiple concurrent quests
- Quest chains (one quest unlocks another)
- Quest items that can only be obtained during the quest
- NPC quest givers
- Quest journal with detailed tracking
- Experience from combat
- Skills/abilities unlocked at certain levels