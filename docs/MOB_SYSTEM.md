# Mob and Equipment System

## Overview

The MUD now features a complete mob (monster) combat system with equipment! Players can fight mobs, gain XP and loot, and equip weapons and armor to grow stronger.

## Mobs (Monsters)

### What are Mobs?

Mobs are non-player enemies that inhabit various rooms in the world. They can be attacked for XP and loot.

### Mob Locations

- **Goblin Scout** - Whispering Wood (east twice from Village Square)
- **Grey Wolf** - Whispering Wood (aggressive!)
- **Skeleton Warrior** - Graveyard (northwest from Village Square)
- **Bandit** - Moonlit Lane (east from Village Square)

### Mob Stats

Each mob has:
- **Health** - How much damage they can take
- **Damage** - How much damage they deal
- **Armor** - Reduces incoming damage
- **XP Reward** - Experience gained when killed
- **Loot Table** - Items they drop when defeated

### Example Mobs

#### Goblin Scout
- Health: 30
- Damage: 8
- Armor: 2
- XP: 50
- Loot: rusty-dagger

#### Skeleton Warrior
- Health: 50
- Damage: 15
- Armor: 5
- XP: 100
- Loot: bone-sword, rusted-chainmail

#### Bandit
- Health: 60
- Damage: 18
- Armor: 8
- XP: 125
- Loot: steel-sword, leather-armor, gold-coins

## Combat System

### Attacking Mobs

**Command:** `attack <mob>`

Example:
```
> attack goblin
You attack a goblin scout for 10 damage!
a goblin scout has 20/30 health remaining.
a goblin scout attacks you for 6 damage!
```

### Combat Mechanics

1. You attack the mob, dealing damage = your total damage - mob's armor (minimum 1)
2. If the mob survives, it counter-attacks, dealing damage = mob's damage - your armor (minimum 1)
3. If you kill the mob:
   - You gain XP (may level up!)
   - The mob drops loot items in the room
   - The mob is removed from the game

### Death

If a mob kills you:
- You respawn in the graveyard
- Your items are left in a corpse at the death location
- Your equipped items are unequipped and added to the corpse

## Equipment System

### Equipment Types

**Weapons** - Increase your damage
**Armor** - Reduce incoming damage

### Equipment Slots

Each player has two equipment slots:
- **Weapon slot** - For wielding weapons
- **Armor slot** - For wearing armor

### Commands

**`equip <item>`** - Equip a weapon or armor from your inventory
```
> equip steel-sword
You wield steel-sword.
```

**`unequip weapon`** or **`unequip armor`** - Remove equipped item
```
> unequip weapon
You unequip steel-sword.
```

**`status`** - View your stats including equipment
```
> status
Hero - Level 2
Health: 110/110  Mana: 55/55
XP: 200/300  (Need 100 more to level)
Damage: 25  Armor: 10
Weapon: steel-sword (+15 damage)
Armor: leather-armor (+10 armor)
```

### Damage Calculation

**Total Damage** = Base Damage (10) + Weapon Damage Bonus

Examples:
- No weapon: 10 damage
- Rusty Dagger (+5): 15 damage
- Steel Sword (+15): 25 damage
- Guardian Axe (+25): 35 damage

### Armor Calculation

**Total Armor** = Armor from equipped armor piece

Examples:
- No armor: 0 armor
- Rusted Chainmail: 5 armor
- Leather Armor: 10 armor
- Nature Amulet: 15 armor

## Available Equipment

### Weapons

| Item | Damage | How to Get |
|------|--------|------------|
| Rusty Dagger | +5 | Kill Goblin Scout |
| Bone Sword | +10 | Kill Skeleton Warrior |
| Steel Sword | +15 | Kill Bandit |
| Guardian Axe | +25 | Kill Forest Guardian (boss) |

### Armor

| Item | Armor | How to Get |
|------|-------|------------|
| Rusted Chainmail | +5 | Kill Skeleton Warrior |
| Leather Armor | +10 | Kill Bandit |
| Nature Amulet | +15 | Kill Forest Guardian (boss) |

### Other Loot

- **Wolf Pelt** - Dropped by wolves (can be sold)
- **Gold Coins** - Dropped by bandits (currency)

## Example Combat Session

```
> e
Moonlit Lane
A narrow lane stretches eastward...

Mobs: a bandit

> look bandit
a bandit: A rough-looking brigand wearing leather armor and carrying a well-maintained sword.
Health: 60/60  Damage: 18  Armor: 8

> status
Hero - Level 2
Health: 110/110  Mana: 55/55
Damage: 10  Armor: 0

> attack bandit
You attack a bandit for 2 damage!
a bandit has 58/60 health remaining.
a bandit attacks you for 18 damage!

> attack bandit
You attack a bandit for 2 damage!
a bandit has 56/60 health remaining.
a bandit attacks you for 18 damage!

... (continue attacking) ...

> attack bandit
You attack a bandit for 2 damage!
You have slain a bandit!
You gained 125 XP!
a bandit dropped: steel-sword, leather-armor, gold-coins

> get steel-sword
You get steel-sword.

> equip steel-sword
You wield steel-sword.

> get leather-armor
You get leather-armor.

> equip leather-armor
You wear leather-armor.

> status
Hero - Level 2
Health: 74/110  Mana: 55/55
XP: 325/300  (Need -25 more to level)
Damage: 25  Armor: 10
Weapon: steel-sword (+15 damage)
Armor: leather-armor (+10 armor)
```

## Tips & Strategies

1. **Start Small** - Fight the goblin first to get your first weapon
2. **Equip Early** - Even a rusty dagger helps a lot!
3. **Watch Your Health** - Mobs hit back! Use health potions if needed
4. **Armor Matters** - Reduces damage significantly in prolonged fights
5. **Level Up First** - Complete the apple quest for easy XP and stats
6. **Boss Fights** - The Forest Guardian is very tough - gear up first!

## Future Enhancements

Potential improvements:
- Multiple mobs per room
- Mob respawning
- Different mob behaviors (fleeing, calling for help)
- Ranged attacks
- Critical hits and dodge mechanics
- Mob-specific abilities
- Better loot probability system (not all items always drop)
- Selling items to vendors
- Crafting system for equipment upgrades