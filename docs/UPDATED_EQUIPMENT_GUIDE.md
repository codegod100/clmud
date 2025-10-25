# Updated Equipment System Guide

## Major Improvements ✨

1. **Equipment stays in inventory** - No more losing items when you equip them!
2. **[EQUIPPED] markers** - Easily see what you're wearing
3. **Item stats shown** - See damage/armor bonuses directly in inventory

## How It Works Now

### Inventory Display

Your inventory now shows:
- **Item count** for stackable items
- **[EQUIPPED]** tag for currently worn items
- **Stats** for weapons and armor ([+X damage] or [+X armor])
- **Item descriptions**

Example:
```
> inventory
Inventory (5 items):
  rusted-chainmail x1 [EQUIPPED] - Old chainmail with rust spots. [+5 armor]
  bone-sword x1 [EQUIPPED] - A sword carved from ancient bones. [+10 damage]
  rusty-dagger x1 - A rusty, chipped dagger. [+5 damage]
  mana-potion x3 - A shimmering blue vial that restores 25 mana.
```

### Equipping Items

Items now **stay in your inventory** when equipped:

```
> inventory
Inventory (4 items):
  rusty-dagger x1 - A rusty, chipped dagger. [+5 damage]
  mana-potion x3 - A shimmering blue vial that restores 25 mana.

> equip rusty-dagger
You wield rusty-dagger.

> inventory
Inventory (4 items):
  rusty-dagger x1 [EQUIPPED] - A rusty, chipped dagger. [+5 damage]
  mana-potion x3 - A shimmering blue vial that restores 25 mana.
```

### Swapping Equipment

When you equip a new item in the same slot, the old one is automatically unequipped:

```
> inventory
Inventory (5 items):
  bone-sword x1 - A sword carved from ancient bones. [+10 damage]
  rusty-dagger x1 [EQUIPPED] - A rusty, chipped dagger. [+5 damage]
  mana-potion x3 - A shimmering blue vial that restores 25 mana.

> equip bone-sword
You wield bone-sword.

> inventory
Inventory (5 items):
  bone-sword x1 [EQUIPPED] - A sword carved from ancient bones. [+10 damage]
  rusty-dagger x1 - A rusty, chipped dagger. [+5 damage]
  mana-potion x3 - A shimmering blue vial that restores 25 mana.
```

Notice: rusty-dagger is no longer [EQUIPPED], bone-sword is now [EQUIPPED]

### Full Session Example

```
> get rusty-dagger
You get rusty-dagger.

> get leather-armor
You get leather-armor.

> inventory
Inventory (5 items):
  leather-armor x1 - Supple leather armor. [+10 armor]
  rusty-dagger x1 - A rusty, chipped dagger. [+5 damage]
  mana-potion x3 - A shimmering blue vial that restores 25 mana.

> equip rusty-dagger
You wield rusty-dagger.

> equip leather-armor
You wear leather-armor.

> inventory
Inventory (5 items):
  leather-armor x1 [EQUIPPED] - Supple leather armor. [+10 armor]
  rusty-dagger x1 [EQUIPPED] - A rusty, chipped dagger. [+5 damage]
  mana-potion x3 - A shimmering blue vial that restores 25 mana.

> status
Knight - Level 1
Health: 85/100  Mana: 50/50
XP: 100/200
Damage: 15  Armor: 10
Weapon: rusty-dagger (5 damage)
Armor: leather-armor (10 armor)

> unequip weapon
You unequip rusty-dagger.

> inventory
Inventory (5 items):
  leather-armor x1 [EQUIPPED] - Supple leather armor. [+10 armor]
  rusty-dagger x1 - A rusty, chipped dagger. [+5 damage]
  mana-potion x3 - A shimmering blue vial that restores 25 mana.
```

## Commands Reference

```
inventory          - Show your inventory with stats and equipped status
inv                - Short form of inventory
i                  - Even shorter!

equip <item>       - Equip a weapon or armor (stays in inventory)
unequip weapon     - Unequip your weapon
unequip armor      - Unequip your armor

status             - See your stats and what's equipped
stats              - Same as status

get <item>         - Pick up item from ground
drop <item>        - Drop item to ground
```

## Benefits of the New System

1. **No Lost Items** - You can't accidentally lose equipment by equipping something else
2. **Easy Comparison** - See stats of all your gear at once
3. **Quick Reference** - Know what's equipped without checking status
4. **Swap Friendly** - Try different loadouts easily
5. **Visual Clarity** - [EQUIPPED] and stat tags make everything obvious

## Available Equipment

### Weapons (+Damage)
| Item | Bonus | Source |
|------|-------|--------|
| rusty-dagger | +5 | Goblins (Whispering Wood) |
| bone-sword | +10 | Skeletons (Graveyard) |
| steel-sword | +15 | Bandits (Moonlit Lane) |
| guardian-axe | +25 | Forest Guardian (Boss) |

### Armor (+Defense)
| Item | Bonus | Source |
|------|-------|--------|
| rusted-chainmail | +5 | Skeletons (Graveyard) |
| leather-armor | +10 | Bandits (Moonlit Lane) |
| nature-amulet | +15 | Forest Guardian (Boss) |

## Tips

1. **Check inventory often** - The new display makes it easy to see everything at a glance
2. **Collect all equipment** - Since items stay in inventory, collect different options
3. **Compare stats** - With stats visible, you can make informed decisions
4. **Try different loadouts** - Easy to swap between light/fast vs. heavy/defensive
5. **The [EQUIPPED] tag** - Makes it impossible to forget what you're wearing
