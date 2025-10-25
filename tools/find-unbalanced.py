#!/usr/bin/env python3
import sys

def find_unbalanced_functions(filename):
    """Find where depth should be 1 (end of top-level forms)."""
    with open(filename, 'r') as f:
        lines = f.readlines()
    
    depth = 0
    in_string = False
    in_comment = False
    escape_next = False
    
    for line_num, line in enumerate(lines, 1):
        line_start_depth = depth
        for char in line:
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
        
        # Lines where we hit depth 1 (should be closing a defun/defvar/etc)
        if depth == 1 and line_start_depth > 1:
            print(f"Line {line_num}: depth {line_start_depth} -> {depth} | {line.rstrip()[:80]}")
    
    print(f"\nFinal depth: {depth}")

if __name__ == '__main__':
    find_unbalanced_functions(sys.argv[1])
