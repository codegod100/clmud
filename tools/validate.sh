#!/bin/bash
# Comprehensive MUD Server Validation Script
# Consolidates all validation functionality

set -e

TOOLS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$TOOLS_DIR")"

cd "$PROJECT_DIR"

echo "=== CLMUD Comprehensive Validation ==="
echo

# Color codes
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

ERRORS=0

# Function to check paren balance using the consolidated tool
check_balance() {
    local file=$1
    if sbcl --script "$TOOLS_DIR/paren-tools.lisp" check "$file" > /dev/null 2>&1; then
        echo -e "${GREEN}✓${NC} $file is balanced"
        return 0
    else
        echo -e "${RED}✗${NC} $file has unbalanced parens"
        sbcl --script "$TOOLS_DIR/paren-tools.lisp" check "$file" 2>&1 | grep -E "(Status:|Missing|Extra)"
        return 1
    fi
}

# Check all source files for paren balance
echo "--- Checking Parenthesis Balance ---"
for file in src/*.lisp; do
    if [ -f "$file" ]; then
        if ! check_balance "$file"; then
            ERRORS=$((ERRORS + 1))
        fi
    fi
done
echo

# Check compilation using consolidated tool
echo "--- Checking Compilation ---"
if sbcl --script "$TOOLS_DIR/compile-tools.lisp" validate > /tmp/mud-validation.log 2>&1; then
    echo -e "${GREEN}✓${NC} All validation checks passed"
else
    EXIT_CODE=$?
    echo -e "${RED}✗${NC} Validation failed (exit code: $EXIT_CODE)"
    ERRORS=$((ERRORS + 1))
    
    # Show errors from log
    if [ -f "/tmp/mud-validation.log" ]; then
        echo -e "${YELLOW}Validation details:${NC}"
        cat /tmp/mud-validation.log
    fi
fi
echo

# Summary
echo "--- Validation Summary ---"
if [ $ERRORS -eq 0 ]; then
    echo -e "${GREEN}All checks passed!${NC}"
    echo
    echo "The server is ready for development and testing."
    exit 0
else
    echo -e "${RED}Found $ERRORS error(s)${NC}"
    echo
    echo "To debug:"
    echo "  - Check balance: sbcl --script tools/paren-tools.lisp check <file>"
    echo "  - Fix balance:   sbcl --script tools/paren-tools.lisp fix <file> --in-place"
    echo "  - Check compile: sbcl --script tools/compile-tools.lisp check"
    echo "  - View log:      less /tmp/mud-validation.log"
    exit 1
fi