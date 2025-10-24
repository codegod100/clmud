# Equipment Guide for Common Lisp MUD

## How to Use Equipment

### Basic Commands

1. **View your inventory:**
   ```
   inventory
   inv
   i
   ```

2. **Pick up items from the ground:**
   ```
   get <item-name>
   ```
   Example: `get rusty-dagger`

3. **Equip a weapon or armor:**
   ```
   equip <item-name>
   ```
   Examples:
   - `equip rusty-dagger`
   - `equip leather-armor`
   - `equip steel-sword`

4. **Unequip items:**
   ```
   unequip weapon
   unequip armor
   ```

5. **Check your stats and equipped items:**
   ```
   status
   stats
   ```

6. **Drop items you don't want:**
   ```
   drop <item-name>
   ```

## Available Weapons

| Weapon | Damage Bonus | Where to Get |
|--------|-------------|--------------|
| rusty-dagger | +5 | Dropped by goblins in Whispering Wood |
| bone-sword | +8 | Dropped by skeletons in Graveyard |
| steel-sword | +12 | Dropped by bandits in Moonlit Lane |
| guardian-axe | +20 | Dropped by Forest Guardian boss |

## Available Armor

| Armor | Defense Bonus | Where to Get |
|-------|--------------|--------------|
| rusted-chainmail | +5 | Dropped by skeletons in Graveyard |
| leather-armor | +10 | Dropped by bandits in Moonlit Lane |
| nature-amulet | +15 | Dropped by Forest Guardian boss |

## How Equipment Works

### Weapons
- **Base damage:** 10
- **With weapon:** Base damage + weapon damage bonus
- Example: With steel-sword (+12), your damage is 22

### Armor
- **Reduces incoming damage** from both mobs and player attacks
- Example: Mob does 18 damage, you have 10 armor = you take 8 damage
- Minimum damage is always 1 (even with high armor)

### Equipment Slots
- **1 weapon slot:** Can only have one weapon equipped at a time
- **1 armor slot:** Can only have one armor equipped at a time
- Equipping a new item automatically unequips the old one and puts it back in your inventory

## Example Gameplay Session

```
> inventory
Inventory (3 items):
  mana-potion x3 - A shimmering blue vial that restores 25 mana.

> status
Warrior - Level 1
Health: 100/100  Mana: 50/50
XP: 0/200  (Need 200 more to level)
Damage: 10  Armor: 0

> move east
> move east
> attack goblin
You attack a goblin scout for 8 damage!
a goblin scout has 22/30 health remaining.
a goblin scout attacks you for 8 damage!

> attack goblin
> attack goblin
> attack goblin
You have slain a goblin scout!
You gained 50 XP!
a goblin scout dropped: rusty-dagger

> get rusty-dagger
You get rusty-dagger.

> equip rusty-dagger
You wield rusty-dagger.

> status
Warrior - Level 1
Health: 76/100  Mana: 50/50
XP: 50/200  (Need 150 more to level)
Damage: 15  Armor: 0
Weapon: rusty-dagger (5 damage)

> attack wolf
You attack a grey wolf for 12 damage!  ← Now doing 15-3 = 12 damage!

> unequip weapon
You unequip rusty-dagger.

> inventory
Inventory (4 items):
  rusty-dagger x1 - A rusty, chipped dagger. Better than nothing.
  mana-potion x3 - A shimmering blue vial that restores 25 mana.
```

## Tips

1. **Collect loot after killing mobs** - Use `look` to see what's on the ground, then `get <item>`
2. **Better equipment = easier fights** - A steel-sword (+12 damage) makes combat much faster
3. **Armor saves your life** - With leather-armor (+10), you take significantly less damage
4. **Equipment stacks** - Weapon damage and armor bonuses add directly to your stats
5. **Check status regularly** - See your current stats and what you have equipped
6. **Boss fights drop the best loot** - The Forest Guardian drops guardian-axe (+20) and nature-amulet (+15)

## Mob Difficulty by Location

- **Whispering Wood:** Goblin (easy), Wolf (medium)
- **Moonlit Lane:** Bandit (medium-hard) - drops steel-sword & leather-armor!
- **Graveyard:** Skeleton (hard) - drops bone-sword & rusted-chainmail
- **Forest (Boss):** Forest Guardian (very hard) - drops guardian-axe & nature-amulet

## Recommended Progression

1. Start by killing goblins to get rusty-dagger
2. Equip rusty-dagger, kill more mobs to level up
3. Kill bandits to get steel-sword and leather-armor
4. With good equipment, tackle skeletons
5. At higher levels with best equipment, challenge the Forest Guardian
