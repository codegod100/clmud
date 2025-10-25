# Complete Equipment Usage Demo

## Quick Reference

### Commands
```
inventory / inv / i     - View your inventory
get <item>             - Pick up item from ground
equip <item>           - Equip weapon or armor
unequip weapon         - Remove equipped weapon
unequip armor          - Remove equipped armor
status                 - View your stats and equipment
drop <item>            - Drop item on ground
```

## Live Example Session

### Starting Out (No Equipment)
```
> status
Warrior - Level 1
Health: 100/100  Mana: 50/50
XP: 0/200  (Need 200 more to level)
Damage: 10  Armor: 0
```

### Getting Your First Weapon
```
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

> look
Whispering Wood
...
Items: rusty-dagger

> get rusty-dagger
You get rusty-dagger.

> inventory
Inventory (4 items):
  rusty-dagger x1 - A rusty, chipped dagger. Better than nothing.
  mana-potion x3 - A shimmering blue vial that restores 25 mana.

> equip rusty-dagger
You wield rusty-dagger.

> status
Warrior - Level 1
Health: 76/100  Mana: 50/50
XP: 50/200  (Need 150 more to level)
Damage: 15  Armor: 0              ← +5 damage from weapon!
Weapon: rusty-dagger (5 damage)
```

### Getting Armor
```
> move northwest
> attack skeleton
You attack a skeleton warrior for 5 damage!  ← Skeleton has 5 armor, so 20-5 = 15 actual damage
a skeleton warrior has 45/50 health remaining.
a skeleton warrior attacks you for 15 damage!

> cast fireball skeleton
You cast fireball at a skeleton warrior for 25 damage!
a skeleton warrior has 20/50 health remaining.
a skeleton warrior attacks you for 15 damage!

> cast fireball skeleton
You have slain a skeleton warrior!
You gained 100 XP!
a skeleton warrior dropped: bone-sword, rusted-chainmail

> get bone-sword
You get bone-sword.

> get rusted-chainmail
You get rusted-chainmail.

> equip bone-sword                ← Replaces rusty-dagger
You wield bone-sword.

> equip rusted-chainmail
You wear rusted-chainmail.

> status
Knight - Level 1
Health: 70/100  Mana: 20/50
XP: 100/200  (Need 100 more to level)
Damage: 20  Armor: 5              ← +10 from weapon, +5 from armor!
Weapon: bone-sword (10 damage)
Armor: rusted-chainmail (5 armor)

> inventory
Inventory (4 items):
  rusty-dagger x1        ← Old weapon went back to inventory
  bone-sword x1
  rusted-chainmail x1
  mana-potion x3
```

### Unequipping Items
```
> unequip weapon
You unequip bone-sword.

> status
Knight - Level 1
Damage: 10  Armor: 5              ← Lost weapon bonus
Armor: rusted-chainmail (5 armor)

> unequip armor
You unequip rusted-chainmail.

> status
Knight - Level 1
Damage: 10  Armor: 0              ← Back to base stats
```

## Equipment Impact Examples

### Without Equipment (Base Stats)
```
Your attack: 10 damage
Mob attacks you: 18 damage
```

### With Rusty Dagger (+5)
```
Your attack: 15 damage           ← Faster kills!
Mob attacks you: 18 damage
```

### With Bone Sword (+10) and Rusted Chainmail (+5)
```
Your attack: 20 damage           ← Much faster kills!
Mob attacks you: 13 damage       ← Take less damage! (18 - 5 armor)
```

### With Best Equipment (Guardian Axe +20, Nature Amulet +15)
```
Your attack: 30 damage           ← Devastating!
Mob attacks you: 3 damage        ← Minimal damage! (18 - 15 armor)
```

## All Equipment Available

### Weapons (Increases Damage)
- **rusty-dagger** (+5) - Easy to get from goblins
- **bone-sword** (+10) - Medium difficulty from skeletons  
- **steel-sword** (+12) - Medium-hard from bandits
- **guardian-axe** (+20) - Very hard boss drop (Forest Guardian in the Ancient Grove)

### Armor (Reduces Damage Taken)
- **rusted-chainmail** (+5 armor) - Medium difficulty from skeletons
- **leather-armor** (+10 armor) - Medium-hard from bandits
- **nature-amulet** (+15 armor) - Very hard boss drop (Forest Guardian in the Ancient Grove)

## Pro Tips

1. **Always pick up loot!** Use `look` after kills, then `get <item>`
2. **Equipment auto-swaps** - Equipping a new weapon puts your old one back in inventory
3. **Status shows everything** - Current HP, mana, XP, damage, armor, and equipped items
4. **Better gear = easier progression** - Each upgrade makes you noticeably stronger
5. **Armor is crucial for tough fights** - Reducing damage is just as important as dealing it
6. **You can carry multiple items** - Keep backups in your inventory
