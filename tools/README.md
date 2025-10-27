# MUD Development Tools

This directory contains utilities for working with the Common Lisp MUD codebase.

## Current Status

✅ **All source files have balanced parentheses**
✅ **Server compiles and runs successfully**
⚠️  **1 non-fatal compilation warning exists** (illegal function call in handle-command)

The server is fully functional despite the compilation warning. The warning indicates a structural issue in the `handle-command` function where SBCL detects code that may be unreachable or improperly structured, but it doesn't prevent compilation or execution.

## ⚠️ IMPORTANT: Fast Development Workflow

**Use fast debugging tools during development - avoid running the full server:**

### Fast Development Cycle (Recommended)
```bash
# 1. Check for issues (FAST - ~1 second)
./dev.sh balance

# 2. Auto-fix parentheses (FAST - ~1 second)
sbcl --script tools/paren-fix.lisp fix <file> --in-place

# 3. Check compilation (FAST - ~2 seconds)
./dev.sh check

# 4. Repeat as needed
```

### Only When Testing Gameplay (SLOW)
```bash
# Full validation with server start (SLOW - ~10+ seconds)
./dev.sh validate

# Start server for gameplay testing (SLOW)
./dev.sh start
```

**Never manually edit parentheses** - this can introduce errors and is time-consuming. The paren-fix tool handles string/comment awareness and ensures correct balancing.

**Never run the full server during development** - use the fast debugging tools instead for quick feedback.

## Parenthesis Checking Tools

### check_parens.lisp
Enhanced parenthesis checker with detailed analysis and string/comment awareness.

**Usage:**
```bash
# Check balance with detailed analysis
sbcl --script tools/check_parens.lisp check src/server/commands.lisp

# Show running depth analysis
sbcl --script tools/check_parens.lisp depth src/server/commands.lisp

# Check all source files
sbcl --script tools/check_parens.lisp all
```

**Features:**
- String and comment awareness (ignores parens in strings/comments)
- Detailed line-by-line analysis
- Running depth tracking
- Reports exact locations of unmatched parentheses
- Handles escape sequences in strings properly

### paren-fix.lisp ⭐ **PRIMARY TOOL**
Enhanced SBCL script that reports imbalance data and optionally rewrites files to balance parentheses. Now includes string and comment awareness.

**This is the RECOMMENDED tool for fixing parenthesis issues - use this instead of manual editing.**

**Usage:**
```bash
# Inspect a file
sbcl --script tools/paren-fix.lisp check src/server/commands.lisp

# Write a balanced copy
sbcl --script tools/paren-fix.lisp fix src/server/commands.lisp /tmp/server-fixed.lisp

# Balance in place (RECOMMENDED - overwrites!)
sbcl --script tools/paren-fix.lisp fix src/server/commands.lisp --in-place
```

**Output:**
```
File: src/server/commands.lisp
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
sbcl --script tools/sexp-edit.lisp list src/server/commands.lisp

# Show form 46 (handle-command)
sbcl --script tools/sexp-edit.lisp show src/server/commands.lisp 46

# Replace a defun
sbcl --script tools/sexp-edit.lisp replace src/server/commands.lisp 10 \
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

### check_parens.lisp
Comprehensive parenthesis checker that validates multiple source files and attempts to read all forms.

**Usage:**
```bash
sbcl --script tools/check_parens.lisp
```

**Features:**
- Checks parenthesis balance in multiple source files
- Attempts to read all top-level forms from each file
- Reports detailed statistics and error locations
- Validates structural integrity of Lisp files

### fix-parens.lisp
Automated parenthesis fixing tool that adds missing opening parentheses to Common Lisp forms.

**Usage:**
```bash
sbcl --script tools/fix-parens.lisp
```

**Features:**
- Automatically detects forms that need opening parentheses
- Handles common Lisp constructs (defun, let, loop, etc.)
- Preserves existing structure and formatting
- Creates fixed output file for review

### fix-handle-look-at.lisp
Specialized tool for fixing the `handle-look-at` function by reading and rewriting it properly.

**Usage:**
```bash
sbcl --script tools/fix-handle-look-at.lisp
```

**Features:**
- Reads and validates the handle-look-at function
- Reformats the function with proper pretty-printing
- Maintains structural integrity during rewriting
- Provides detailed feedback on function structure

### check-server.lisp
Server inspection tool that loads and examines server source files for function definitions.

**Usage:**
```bash
sbcl --script tools/check-server.lisp
```

**Features:**
- Loads server packages and core files
- Inspects function definitions in server modules
- Reports successful form reading statistics
- Validates server code structure

### compile_server.lisp
Simple compilation utility for server source files.

**Usage:**
```bash
sbcl --script tools/compile_server.lisp
```

**Features:**
- Compiles core server files in proper order
- Handles package loading and dependencies
- Provides compilation feedback

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
   python3 tools/lisp-safe-edit.py src/server/commands.lisp
   # OR
   sbcl --script tools/sexp-edit.lisp check src/server/commands.lisp
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
