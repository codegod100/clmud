#!/usr/bin/env python3
import sys
import re

def count_parens(text):
    """Count parentheses, ignoring those in strings and comments."""
    depth = 0
    in_string = False
    in_comment = False
    escape_next = False
    
    for char in text:
        if escape_next:
            escape_next = False
            continue
            
        if in_comment:
            if char == '\n':
                in_comment = False
            continue
            
        if in_string:
            if char == '\\':
                escape_next = True
            elif char == '"':
                in_string = False
            continue
            
        if char == ';':
            in_comment = True
        elif char == '"':
            in_string = True
        elif char == '(':
            depth += 1
        elif char == ')':
            depth -= 1
            
    return depth

def check_balance(filename):
    """Check if parentheses are balanced in file."""
    with open(filename, 'r') as f:
        content = f.read()
    return count_parens(content)

def safe_replace(filename, start_line, end_line, new_content):
    """Safely replace lines in a file, checking paren balance."""
    # Read original file
    with open(filename, 'r') as f:
        lines = f.readlines()
    
    # Check original balance
    orig_balance = count_parens(''.join(lines))
    
    # Create new content
    before = lines[:start_line-1]
    after = lines[end_line:]
    new_lines = before + [new_content + '\n'] + after
    new_text = ''.join(new_lines)
    
    # Check new balance
    new_balance = count_parens(new_text)
    
    # Calculate what we're changing
    old_section = ''.join(lines[start_line-1:end_line])
    old_section_balance = count_parens(old_section)
    new_content_balance = count_parens(new_content)
    
    print(f"Original file balance: {orig_balance}")
    print(f"Old section balance: {old_section_balance}")
    print(f"New content balance: {new_content_balance}")
    print(f"Balance change: {new_content_balance - old_section_balance}")
    print(f"New file balance: {new_balance}")
    
    if new_balance == 0:
        print("✓ File will be balanced after change")
        with open(filename, 'w') as f:
            f.write(new_text)
        print(f"✓ Updated {filename}")
        return True
    else:
        print(f"✗ File would be unbalanced (depth {new_balance}), change NOT applied")
        return False

if __name__ == '__main__':
    if len(sys.argv) < 2:
        print("Usage:")
        print("  Check balance: python3 lisp-safe-edit.py <file>")
        print("  Replace lines: python3 lisp-safe-edit.py <file> <start> <end> <new_content>")
        sys.exit(1)
    
    filename = sys.argv[1]
    
    if len(sys.argv) == 2:
        # Just check balance
        balance = check_balance(filename)
        if balance == 0:
            print(f"✓ {filename} is balanced")
        else:
            print(f"✗ {filename} has unbalanced parens (depth {balance})")
        sys.exit(0 if balance == 0 else 1)
    
    if len(sys.argv) == 5:
        start_line = int(sys.argv[2])
        end_line = int(sys.argv[3])
        new_content = sys.argv[4]
        safe_replace(filename, start_line, end_line, new_content)
