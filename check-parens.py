#!/usr/bin/env python3
"""
Paren balance checker for Lisp files.
Reports the exact line and column where parentheses become unbalanced.
"""

import sys


def check_parens(filename):
    """Check parenthesis balance in a Lisp file."""
    with open(filename, "r") as f:
        lines = f.readlines()

    stack = []  # Stack of (line_num, col_num, char)
    in_string = False
    in_comment = False
    escape_next = False

    for line_num, line in enumerate(lines, 1):
        col_num = 0
        for char in line:
            col_num += 1

            # Handle escape sequences in strings
            if escape_next:
                escape_next = False
                continue

            if char == "\\" and in_string:
                escape_next = True
                continue

            # Handle strings
            if char == '"':
                in_string = not in_string
                continue

            # Skip everything inside strings
            if in_string:
                continue

            # Handle line comments
            if char == ";":
                in_comment = True
                continue

            # Skip everything in comments
            if in_comment:
                continue

            # Check parentheses
            if char == "(":
                stack.append((line_num, col_num, char))
            elif char == ")":
                if not stack:
                    print(
                        f"ERROR: Unmatched closing paren at line {line_num}, column {col_num}"
                    )
                    print(f"  {line.rstrip()}")
                    print(f"  {' ' * (col_num - 1)}^")
                    return False
                stack.pop()

        # Reset comment flag at end of line
        in_comment = False

    # Check for unclosed strings
    if in_string:
        print("ERROR: Unclosed string at end of file")
        return False

    # Check for unclosed parentheses
    if stack:
        print(f"ERROR: {len(stack)} unclosed opening paren(s)")
        print("\nUnclosed parens at:")
        for line_num, col_num, char in stack[-5:]:  # Show last 5
            line = lines[line_num - 1]
            print(f"  Line {line_num}, column {col_num}:")
            print(f"    {line.rstrip()}")
            print(f"    {' ' * (col_num - 1)}^")
        if len(stack) > 5:
            print(f"  ... and {len(stack) - 5} more")
        return False

    print(f"✓ All parentheses balanced! ({filename})")
    return True


def main():
    if len(sys.argv) < 2:
        print("Usage: python3 check-parens.py <file.lisp>")
        sys.exit(1)

    filename = sys.argv[1]

    try:
        success = check_parens(filename)
        sys.exit(0 if success else 1)
    except FileNotFoundError:
        print(f"ERROR: File not found: {filename}")
        sys.exit(1)
    except Exception as e:
        print(f"ERROR: {e}")
        sys.exit(1)


if __name__ == "__main__":
    main()
