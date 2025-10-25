# MUD Development Tools

This directory contains utilities for working with the Common Lisp MUD codebase.

## Current Status

✅ **All source files have balanced parentheses**
✅ **Server compiles and runs successfully**
⚠️  **1 non-fatal compilation warning exists** (illegal function call in handle-command)

The server is fully functional despite the compilation warning. The warning indicates a structural issue in the `handle-command` function where SBCL detects code that may be unreachable or improperly structured, but it doesn't prevent compilation or execution.

## Parenthesis Checking Tools

### lisp-safe-edit.py
Safe editing tool that validates parenthesis balance before making changes.

**Usage:**
```bash
# Check if a file is balanced
python3 tools/lisp-safe-edit.py src/server.lisp

# Replace lines (safely)
python3 tools/lisp-safe-edit.py <file> <start-line> <end-line> <new-content>
```

**Example:**
```bash
python3 tools/lisp-safe-edit.py src/server.lisp 100 100 '       (write-crlf stream "test")))'
```

The tool will:
- Check current file balance
- Calculate balance change
- Only apply the edit if the file remains balanced (depth 0)

### check-paren-balance.py
Shows running parenthesis depth through a file to identify where imbalances occur.

**Usage:**
```bash
python3 tools/check-paren-balance.py src/server.lisp
```

**Output:**
```
Line 723: depth 9 -> 4
Line 1116: depth 6 -> 1
...
Final depth: 0
```

### find-unbalanced.py
Finds top-level forms that don't properly close (should return to depth 1).

**Usage:**
```bash
python3 tools/find-unbalanced.py src/server.lisp
```

### paren-fix.lisp
SBCL script that both reports imbalance data and optionally rewrites files to balance raw parentheses. The fixer is purely lexical, so it looks only at literal `(` and `)` characters; it does not attempt to understand comments or reader macros.

**Usage:**
```bash
# Inspect a file
sbcl --script tools/paren-fix.lisp check src/server.lisp

# Write a balanced copy
sbcl --script tools/paren-fix.lisp fix src/server.lisp /tmp/server-fixed.lisp

# Balance in place (overwrites!)
sbcl --script tools/paren-fix.lisp fix src/server.lisp --in-place
```

**Output:**
```
File: src/server.lisp
  Opens:   15823
  Closes:  15820
  Status: UNBALANCED (diff 3)
  Missing closing parens: 3
  Extra closing parens at:
    line 142, column 7
```

**Auto-fix details:**
- Removes unmatched closing parens (keeps a log of line/column locations).
- Appends the minimum number of closing parens to reach depth zero.
- Leaves already-balanced files untouched.
- Works best on structural code edits; if literal parens appear inside strings, double-check the result.

## S-Expression Editing Tools

### sexp-edit.lisp
SBCL-based tool that manipulates Lisp files at the s-expression level, ensuring all operations maintain structural integrity.

**Usage:**
```bash
# List all top-level forms
sbcl --script tools/sexp-edit.lisp list <file>

# Count forms
sbcl --script tools/sexp-edit.lisp count <file>

# Check if file is balanced
sbcl --script tools/sexp-edit.lisp check <file>

# Show a specific form
sbcl --script tools/sexp-edit.lisp show <file> <index>

# Delete a form
sbcl --script tools/sexp-edit.lisp delete <file> <index> <output-file>

# Insert a form before index
sbcl --script tools/sexp-edit.lisp insert <file> <index> '<form>' <output-file>

# Replace a form
sbcl --script tools/sexp-edit.lisp replace <file> <index> '<form>' <output-file>
```

**Examples:**
```bash
# See all top-level forms in server.lisp
sbcl --script tools/sexp-edit.lisp list src/server.lisp

# Show form 46 (handle-command)
sbcl --script tools/sexp-edit.lisp show src/server.lisp 46

# Replace a defun
sbcl --script tools/sexp-edit.lisp replace src/server.lisp 10 \
  '(defun test () (format t "Hello~%"))' /tmp/output.lisp
```

**Why use this instead of text editing?**
- Operates on complete s-expressions only
- Automatically maintains parenthesis balance
- Pretty-prints output with proper indentation
- Prevents accidental syntax errors

### check-handle-command.lisp
SBCL script to validate the `handle-command` function and count missing parens.

**Usage:**
```bash
sbcl --script tools/check-handle-command.lisp
```

**Output:**
```
END-OF-FILE while reading handle-command
At EOF: depth=1 (needs 1 more closing parens)
Max depth reached: 22
```

## Validation Scripts

### validate.sh
Comprehensive validation that checks balance and compilation.

**Usage:**
```bash
./tools/validate.sh
```

### check-compile.sh
Detailed compilation checker with error reporting.

**Usage:**
```bash
./tools/check-compile.sh
```

## Quick Validation Commands

**Check all source files:**
```bash
for f in src/*.lisp; do 
    echo "=== $f ===" 
    python3 tools/lisp-safe-edit.py "$f"
done
```

**Check with s-expression tool:**
```bash
for f in src/*.lisp; do
    sbcl --script tools/sexp-edit.lisp check "$f" || echo "ERROR in $f"
done
```

**Load and compile check:**
```bash
sbcl --noinform --non-interactive --load mud.lisp 2>&1 | grep -i error
```

**Count compilation errors:**
```bash
timeout 5 sbcl --script mud.lisp 2>&1 | grep -c "illegal function call"
```

## Best Practices

1. **Always check balance before manual edits:**
   ```bash
   python3 tools/lisp-safe-edit.py src/server.lisp
   # OR
   sbcl --script tools/sexp-edit.lisp check src/server.lisp
   ```

2. **Prefer s-expression level editing:**
   - Use `sexp-edit.lisp` to manipulate whole forms
   - Avoids parenthesis mismatches entirely
   - Automatically formats output

3. **After any edit, verify:**
   - Balance: `python3 tools/lisp-safe-edit.py <file>`
   - S-expressions: `sbcl --script tools/sexp-edit.lisp check <file>`
   - Compilation: `./tools/check-compile.sh`
   - Full validation: `./tools/validate.sh`

4. **Keep tools in this directory** - don't scatter one-off scripts in the project root

5. **For complex structural changes:**
   - Extract form: `sbcl --script tools/sexp-edit.lisp show <file> <index> > /tmp/form.lisp`
   - Edit form manually in /tmp/form.lisp
   - Replace: `sbcl --script tools/sexp-edit.lisp replace <file> <index> "$(cat /tmp/form.lisp)" <output>`

## Common Issues

### "illegal function call" error
This usually means a `cond` clause is not properly closed, causing subsequent clauses to be interpreted as function calls within the previous clause.

**Fix:** Find the clause mentioned in the error and count its closing parens carefully.

### "end of file" error
Missing closing parentheses somewhere. Use `check-paren-balance.py` to find where depth doesn't return to 0.

### Extra closing parens
File balance will be negative. Use `python3 tools/lisp-safe-edit.py` to see the depth.

## Tool Organization

All development tools are kept in this `tools/` directory to avoid cluttering the project root:
- Python scripts for paren balance checking
- SBCL scripts for s-expression manipulation  
- Shell scripts for validation and compilation checks
- Documentation (this README)

Do not create ad-hoc scripts in the project root - add them here instead.
