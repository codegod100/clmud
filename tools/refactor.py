#!/usr/bin/env python3
"""
Apply refactoring to handle-command in server.lisp
This script replaces large inline command implementations with calls to helper functions.
Uses manual line ranges to avoid paren-matching errors.
"""

import sys


def apply_refactoring(input_file, output_file):
    """Apply all refactorings to the file"""

    with open(input_file, "r") as f:
        lines = f.readlines()

    # Define replacements with exact line ranges (0-indexed, so subtract 1 from line numbers)
    # These ranges were determined manually from the source file
    replacements = [
        {
            "start": 1029,  # Line 1030 in editor (attack command)
            "end": 1119,  # Line 1120 in editor
            "new_code": [
                '      ((string= verb "attack")\n',
                "       (if (zerop (length rest))\n",
                '           (write-crlf (player-stream player) (wrap "Attack what? Usage: attack <target>" :bright-red))\n',
                "           (handle-attack-command player (string-trim '(#\\Space #\\Tab) rest))))\n",
            ],
        },
        {
            "start": 1120,  # Line 1121 in editor (ram command)
            "end": 1222,  # Line 1223 in editor
            "new_code": [
                '      ((string= verb "ram")\n',
                "       (if (zerop (length rest))\n",
                '           (write-crlf (player-stream player) (wrap "Ram what? Usage: ram <target>" :bright-red))\n',
                "           (handle-ram-command player (string-trim '(#\\Space #\\Tab) rest))))\n",
            ],
        },
        {
            "start": 1223,  # Line 1224 in editor (bomb command)
            "end": 1297,  # Line 1298 in editor
            "new_code": [
                '      ((string= verb "bomb")\n',
                "       (handle-bomb-command player))\n",
            ],
        },
        {
            "start": 1298,  # Line 1299 in editor (equip command)
            "end": 1362,  # Line 1363 in editor
            "new_code": [
                '      ((string= verb "equip")\n',
                "       (if (zerop (length rest))\n",
                '           (write-crlf (player-stream player) (wrap "Equip what? Usage: equip <item> or equip all" :bright-red))\n',
                "           (let ((item-name (string-trim '(#\\Space #\\Tab) rest)))\n",
                '             (if (string-equal item-name "all")\n',
                "                 (handle-equip-all-command player)\n",
                "                 (handle-equip-item-command player item-name)))))\n",
            ],
        },
        {
            "start": 1409,  # Line 1410 in editor (get/grab command)
            "end": 1525,  # Line 1526 in editor
            "new_code": [
                '      ((member verb \'("get" "grab") :test #\'string=)\n',
                "       (if (zerop (length rest))\n",
                '           (write-crlf (player-stream player) (wrap "Get what? Usage: get <item> or get all" :bright-red))\n',
                "           (let ((item-name (string-trim '(#\\Space #\\Tab) rest)))\n",
                '             (if (string-equal item-name "all")\n',
                "                 (handle-get-all-command player)\n",
                "                 (handle-get-item-command player item-name))))))\n",
            ],
        },
    ]

    # Sort replacements by start line in reverse order so we can apply them
    # from bottom to top without messing up line numbers
    replacements.sort(key=lambda x: x["start"], reverse=True)

    # Apply replacements
    new_lines = lines[:]
    total_removed = 0
    for repl in replacements:
        start = repl["start"]
        end = repl["end"]
        new_code = repl["new_code"]

        old_count = end - start + 1
        new_count = len(new_code)
        removed = old_count - new_count

        # Replace the lines
        new_lines = new_lines[:start] + new_code + new_lines[end + 1 :]
        print(
            f"Replaced lines {start + 1}-{end + 1} ({old_count} lines) with {new_count} new lines (saved {removed} lines)"
        )
        total_removed += removed

    # Write output
    with open(output_file, "w") as f:
        f.writelines(new_lines)

    print(f"\n✓ Refactoring complete! Written to {output_file}")
    print(f"  Original: {len(lines)} lines")
    print(f"  Refactored: {len(new_lines)} lines")
    print(
        f"  Saved: {total_removed} lines ({total_removed * 100 // len(lines)}% reduction)"
    )


if __name__ == "__main__":
    if len(sys.argv) != 3:
        print("Usage: python3 refactor.py input_file output_file")
        print("Example: python3 refactor.py src/server.lisp src/server.lisp.new")
        sys.exit(1)

    input_file = sys.argv[1]
    output_file = sys.argv[2]

    apply_refactoring(input_file, output_file)
