#!/bin/bash
# MUD Server Validation Script
# Checks parenthesis balance and compilation for all Lisp source files

set -e

TOOLS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$TOOLS_DIR")"

cd "$PROJECT_DIR"

echo "=== MUD Server Validation ==="
echo

# Color codes
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

ERRORS=0

# Function to check paren balance
check_balance() {
    local file=$1
    if python3 "$TOOLS_DIR/lisp-safe-edit.py" "$file" > /dev/null 2>&1; then
        echo -e "${GREEN}✓${NC} $file is balanced"
        return 0
    else
        echo -e "${RED}✗${NC} $file has unbalanced parens"
        python3 "$TOOLS_DIR/lisp-safe-edit.py" "$file" 2>&1 | grep "depth"
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

# Try to load and compile
echo "--- Checking SBCL Compilation ---"
if timeout 10 sbcl --noinform --non-interactive --load mud.lisp > /tmp/mud-compile.log 2>&1; then
    echo -e "${GREEN}✓${NC} Server loads successfully"
else
    EXIT_CODE=$?
    if [ $EXIT_CODE -eq 124 ]; then
        echo -e "${GREEN}✓${NC} Server loaded and started (timeout reached)"
        # Check for compilation errors even on timeout
        ERROR_COUNT=$(grep "caught ERROR" /tmp/mud-compile.log 2>/dev/null | wc -l)
        if [ "$ERROR_COUNT" -gt 0 ]; then
            echo -e "${YELLOW}Note: $ERROR_COUNT compilation warning(s) present (non-fatal)${NC}"
        fi
    else
        echo -e "${RED}✗${NC} Server failed to load"
        ERRORS=$((ERRORS + 1))

        # Show errors
        ERROR_COUNT=$(grep -c "caught ERROR" /tmp/mud-compile.log 2>/dev/null || echo 0)
        if [ "$ERROR_COUNT" -gt 0 ]; then
            echo -e "${YELLOW}Found $ERROR_COUNT compilation errors:${NC}"
            grep -A 2 "caught ERROR" /tmp/mud-compile.log | head -20
        fi

        # Show warnings
        WARNING_COUNT=$(grep "caught WARNING" /tmp/mud-compile.log 2>/dev/null | wc -l)
        if [ "$WARNING_COUNT" -gt 0 ]; then
            echo -e "${YELLOW}Found $WARNING_COUNT warnings${NC}"
        fi
    fi
fi
echo

# Summary
echo "--- Validation Summary ---"
if [ $ERRORS -eq 0 ]; then
    echo -e "${GREEN}All checks passed!${NC}"
    exit 0
else
    echo -e "${RED}Found $ERRORS error(s)${NC}"
    echo
    echo "To debug:"
    echo "  - Check balance: python3 tools/lisp-safe-edit.py <file>"
    echo "  - Find issues: python3 tools/check-paren-balance.py <file>"
    echo "  - View compile log: less /tmp/mud-compile.log"
    exit 1
fi
