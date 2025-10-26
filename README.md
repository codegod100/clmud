# CLMUD - A Text Adventure Game

Welcome to **CLMUD** (Common Lisp MUD), a classic text-based adventure game where you explore a fantasy world, battle monsters, complete quests, and grow stronger through your adventures!

## What is CLMUD?

CLMUD is a **Multi-User Dungeon** (MUD) - a text-based online game where you play as an adventurer exploring a rich fantasy world. Think of it like a digital version of a tabletop role-playing game, but played entirely through text commands.

### What You Can Do

🌲 **Explore a Living World**
- Wander through the Village Square, Whispering Wood, Ancient Grove, and more
- Discover hidden locations and secret passages
- Examine interesting objects and learn their stories

⚔️ **Battle Monsters**
- Fight goblins, skeletons, wolves, and bandits
- Use weapons and armor to become stronger
- Cast spells like fireball and healing magic

📜 **Complete Quests**
- Help the village elder by finding fresh apples
- Gain experience points and level up
- Unlock new abilities as you grow stronger

🎒 **Manage Your Adventure**
- Collect weapons, armor, and magical items
- Trade with merchants for better equipment
- Ride vehicles for fast travel across the world

## Quick Start Guide

### 1. Start the Game Server

```bash
./dev.sh start
```

The server will start and tell you it's listening on port 4000.

### 2. Connect to the Game

Open a new terminal and connect:

```bash
telnet localhost 4000
```

You'll see the game welcome you and ask for your name!

### 3. Start Playing

Once connected, you can immediately start exploring:

```
> look
> move north
> inventory
> status
```

## Basic Commands

### Movement
- `north`, `south`, `east`, `west` (or `n`, `s`, `e`, `w`) - Move in directions
- `up`, `down` - Move between floors
- `look` - Examine your current location
- `examine <thing>` - Look closely at objects or people

### Combat
- `attack <monster>` - Fight a monster
- `cast fireball <target>` - Cast a damage spell
- `cast heal` - Heal yourself with magic

### Items & Equipment
- `inventory` (or `inv`, `i`) - See what you're carrying
- `get <item>` - Pick up items from the ground
- `equip <item>` - Wear armor or wield weapons
- `drop <item>` - Leave items behind
- `status` - See your health, level, and equipment

### Quests
- `quest` - See your current quests
- `quest start apple` - Begin the apple-picking quest

## Your First Adventure

Here's a suggested path for new players:

1. **Start in the Village Square** - Look around and get familiar
2. **Visit the Tavern** - Go north to "The Bronze Badger" and examine the map
3. **Find the Garden** - Go south to find the village garden with apples
4. **Start Your First Quest** - Type `quest start apple` then `get apple`
5. **Explore the Woods** - Go east twice to reach Whispering Wood
6. **Fight Your First Monster** - Attack the goblin scout you find there
7. **Get Better Equipment** - Pick up the rusty dagger the goblin drops
8. **Equip Your New Weapon** - Type `equip rusty-dagger` and check your status

## Game Features

### 🏰 **Rich World**
- **Village Square** - The heart of the community with an ancient oak tree
- **The Bronze Badger** - A cozy tavern with a mysterious map
- **Whispering Wood** - A forest filled with secrets and danger
- **Ancient Grove** - Home to the powerful Forest Guardian
- **Sky Platforms** - Floating islands accessible by air vehicles

### ⚔️ **Combat System**
- Turn-based combat with monsters
- Different monster types with unique abilities
- Equipment affects your damage and defense
- Spells for both offense and healing

### 📈 **Character Progression**
- Gain experience points (XP) from quests and combat
- Level up to increase health and mana
- Find better weapons and armor as you explore
- Each level makes you noticeably stronger

### 🎒 **Equipment System**
- **Weapons** increase your damage (rusty dagger → steel sword → guardian axe)
- **Armor** reduces damage you take (leather armor → chainmail → nature amulet)
- Equipment stays in your inventory when equipped
- Better gear makes tough fights much easier

### 🚗 **Vehicles**
- Enter vehicles like boats and flying machines
- Use `uber` command for fast travel between locations
- Some vehicles can be used in combat with the `ram` command

## Tips for New Players

1. **Always check your status** - Use `status` to see your health, level, and equipment
2. **Pick up everything** - Items dropped by monsters are usually useful
3. **Equipment matters** - Even a rusty dagger makes you much stronger
4. **Explore thoroughly** - Use `look` and `examine` to find hidden details
5. **Don't be afraid to die** - You respawn in the graveyard and can recover your items
6. **Talk to NPCs** - Some characters have useful information or quests

## Technical Details

CLMUD is built in Common Lisp and runs as a network server. Players connect via telnet or netcat to play together in the same world. The game features:

- Persistent world that saves between sessions
- Real-time multiplayer (multiple players can be online simultaneously)
- Comprehensive command system with natural language parsing
- Rich text descriptions with ANSI color support
- Modular codebase for easy expansion

## Development

If you're interested in contributing to CLMUD or understanding how it works, check out the `docs/` directory for detailed technical documentation, or look at the source code in `src/`.

## Have Fun!

CLMUD is designed to be enjoyed at your own pace. Whether you want to explore every corner of the world, become the strongest warrior, or just relax in the tavern chatting with other players, there's something for everyone.

Welcome to your adventure! 🌟
