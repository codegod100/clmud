#!/usr/bin/env python3
import sys

def count_parens_running(filename):
    """Show running paren balance."""
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
        
        # Report lines where depth changes significantly or goes back to 0
        if depth != line_start_depth and (depth == 0 or abs(depth - line_start_depth) > 2 or depth < 0):
            print(f"Line {line_num}: depth {line_start_depth} -> {depth}")
    
    print(f"\nFinal depth: {depth}")

if __name__ == '__main__':
    count_parens_running(sys.argv[1])
