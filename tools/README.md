# MUD Development Tools

This directory contains consolidated utilities for working with the Common Lisp MUD codebase.

## Current Status

✅ **All source files have balanced parentheses**
✅ **Server compiles and runs successfully**
✅ **Tools have been consolidated and optimized**

## ⚠️ IMPORTANT: Fast Development Workflow

**Use fast debugging tools during development - avoid running the full server:**

### Fast Development Cycle (Recommended)
```bash
# 1. Check for issues (FAST - ~1 second)
./dev.sh balance

# 2. Auto-fix parentheses (FAST - ~1 second)
sbcl --script tools/paren-tools.lisp fix <file> --in-place

# 3. Check compilation (FAST - ~2 seconds)
./dev.sh check

# 4. DONE! No server needed for compilation checking.
```

### 🤖 AI Agent Instructions

**⚠️ CRITICAL FOR AI AGENTS: After running `./dev.sh check` - DO NOT run `sbcl --script mud.lisp`!**

**Common AI mistake pattern:**
1. Run `./dev.sh check` ✅
2. See "compilation successful" ✅  
3. **INCORRECTLY** run `sbcl --script mud.lisp` ❌

**Correct AI workflow:**
1. Run `./dev.sh check` ✅
2. See "compilation successful" ✅
3. **STOP** - compilation check is complete ✅

**❌ AI AGENTS - DON'T do this:**
```bash
./dev.sh check
sbcl --script mud.lisp  # WRONG! Server not needed for compilation check
```

```bash
./dev.sh balance
sbcl --script mud.lisp  # WRONG! Server not needed for balance check
```

```bash
sbcl --script tools/paren-tools.lisp fix file.lisp --in-place
sbcl --script mud.lisp  # WRONG! Server not needed after fixing parentheses
```

**✅ AI AGENTS - DO this instead:**
```bash
./dev.sh balance
./dev.sh check
# DONE! No server needed for development debugging.
```

**When AI should run the server:**
- Only when user explicitly asks to test gameplay
- Only when user says "test the server" or "start the game"
- Only for final integration testing after major changes

**When AI should NOT run the server:**
- After `./dev.sh check` (compilation verification)
- After `./dev.sh balance` (parenthesis checking)  
- After `paren-tools.lisp` (parenthesis fixing)
- For syntax validation
- For code structure verification

### Only When Testing Gameplay (SLOW)
```bash
# Full validation with server start (SLOW - ~10+ seconds)
./dev.sh validate

# Start server for gameplay testing (SLOW)
./dev.sh start
```

**Never manually edit parentheses** - this can introduce errors and is time-consuming. The paren-tools handles string/comment awareness and ensures correct balancing.

**Never run `mud.lisp` or `sbcl --script mud.lisp` for debugging** - it's too slow! Use the fast debugging tools instead.

**Never run the full server during development** - use the fast debugging tools instead for quick feedback.

## Consolidated Tools

### paren-tools.lisp ⭐ **PRIMARY PARENTHESIS TOOL**
Comprehensive parenthesis checking and fixing tool with string and comment awareness.

**Usage:**
```bash
# Check balance with detailed analysis
sbcl --script tools/paren-tools.lisp check src/server/commands.lisp

# Show running depth analysis
sbcl --script tools/paren-tools.lisp depth src/server/commands.lisp

# Fix parentheses in place (RECOMMENDED)
sbcl --script tools/paren-tools.lisp fix src/server/commands.lisp --in-place

# Fix to new file
sbcl --script tools/paren-tools.lisp fix src/server/commands.lisp /tmp/fixed.lisp

# Check all source files
sbcl --script tools/paren-tools.lisp all
```

**Features:**
- String and comment awareness (ignores parens in strings/comments)
- Detailed line-by-line analysis
- Running depth tracking
- Reports exact locations of unmatched parentheses
- Handles escape sequences in strings properly
- Auto-fix with detailed reporting

### compile-tools.lisp ⭐ **PRIMARY COMPILATION TOOL**
Comprehensive compilation checking and validation tool.

**Usage:**
```bash
# Check compilation of all source files
sbcl --script tools/compile-tools.lisp check

# Check server function loading
sbcl --script tools/compile-tools.lisp functions

# Run comprehensive validation
sbcl --script tools/compile-tools.lisp validate
```

**Features:**
- Loads all source files in correct order
- Reports compilation errors and warnings
- Validates server function definitions
- Comprehensive validation combining all checks

### validate.sh ⭐ **PRIMARY VALIDATION SCRIPT**
Comprehensive validation script that checks balance and compilation.

**Usage:**
```bash
# Run full validation
./tools/validate.sh
```

**Features:**
- Checks parenthesis balance for all source files
- Validates compilation using compile-tools.lisp
- Color-coded output for easy reading
- Detailed error reporting and debugging suggestions

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

## Quick Validation Commands

**Check all source files:**
```bash
for f in src/*.lisp; do 
    echo "=== $f ===" 
    sbcl --script tools/paren-tools.lisp check "$f"
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
   sbcl --script tools/paren-tools.lisp check src/server/commands.lisp
   ```

2. **Prefer s-expression level editing:**
   - Use `sexp-edit.lisp` to manipulate whole forms
   - Avoids parenthesis mismatches entirely
   - Automatically formats output

3. **After any edit, verify:**
   - Balance: `sbcl --script tools/paren-tools.lisp check <file>`
   - S-expressions: `sbcl --script tools/sexp-edit.lisp check <file>`
   - Compilation: `sbcl --script tools/compile-tools.lisp check`
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
Missing closing parentheses somewhere. Use `paren-tools.lisp` to find where depth doesn't return to 0.

### Extra closing parens
File balance will be negative. Use `sbcl --script tools/paren-tools.lisp check` to see the depth.

## Tool Organization

All development tools are kept in this `tools/` directory to avoid cluttering the project root:
- **paren-tools.lisp** - Comprehensive parenthesis checking and fixing
- **compile-tools.lisp** - Compilation checking and validation
- **sexp-edit.lisp** - S-expression manipulation
- **validate.sh** - Complete validation script
- Documentation (this README)

## Refactoring Summary

The tools directory has been significantly refactored to eliminate redundancy:

**Removed (10 redundant tools):**
- `check_parens.lisp` (duplicate)
- `check_parens_new.lisp` (duplicate)
- `fix-parens.lisp` (superseded)
- `sbcl-simple-fix.lisp` (superseded)
- `sbcl-advanced-fix.lisp` (superseded)
- `sbcl-fix-parens.lisp` (superseded)
- `smart-fix-parens.lisp` (superseded)
- `check-compile.sh` (superseded)
- `compile_server.lisp` (superseded)
- `check-server.lisp` (superseded)
- `check-handle-command.lisp` (specialized, not needed)
- `fix-handle-look-at.lisp` (specialized, not needed)

**Consolidated into (4 comprehensive tools):**
- `paren-tools.lisp` - All parenthesis operations
- `compile-tools.lisp` - All compilation operations
- `sexp-edit.lisp` - S-expression manipulation (kept as-is)
- `validate.sh` - Complete validation (updated)

This reduces the tools directory from 16 files to 6 files while maintaining all functionality and improving usability.