#!/bin/bash
# Detailed SBCL compilation checker
# Shows all compilation errors and warnings

TOOLS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$TOOLS_DIR")"

cd "$PROJECT_DIR"

LOG_FILE="/tmp/mud-compile-detail.log"

echo "=== Detailed Compilation Check ==="
echo

# Create temporary compilation script
cat > /tmp/mud-compile-check.lisp << 'EOF'
(load "src/packages.lisp")
(load "src/ansi.lisp")
(load "src/world.lisp")
(load "src/inventory.lisp")
(load "src/player.lisp")
(load "src/merchant.lisp")
(load "src/mob.lisp")
(load "src/combat.lisp")
(load "src/quest.lisp")
(load "src/server/core.lisp")
(load "src/server/commands.lisp")
(load "src/server/runtime.lisp")
(quit)
EOF

# Run SBCL compilation check without starting server
timeout 10 sbcl --noinform --non-interactive --script /tmp/mud-compile-check.lisp > "$LOG_FILE" 2>&1
EXIT_CODE=$?

# Check exit code
if [ $EXIT_CODE -eq 124 ]; then
    echo "✓ Compilation completed successfully (timeout reached)"
    echo
elif [ $EXIT_CODE -eq 0 ]; then
    echo "✓ All files compiled without errors"
    echo
else
    echo "✗ Compilation failed (exit code: $EXIT_CODE)"
    echo
fi

# Count and show errors
if [ -f "$LOG_FILE" ]; then
    ERROR_COUNT=$(grep -c "caught ERROR" "$LOG_FILE" 2>/dev/null)
    GREP_STATUS=$?
    if [ $GREP_STATUS -ne 0 ]; then
        ERROR_COUNT=0
    fi
else
    ERROR_COUNT=0
fi

if [ "$ERROR_COUNT" -gt 0 ]; then
    echo "=== Compilation Errors ($ERROR_COUNT) ==="
    grep -B 15 "caught ERROR" "$LOG_FILE" | grep -v "^--$"
    echo
fi

# Count and show warnings (optional, commented out by default as they're usually harmless)
# WARNING_COUNT=$(grep -c "caught WARNING" /tmp/mud-compile-detail.log 2>/dev/null || echo 0)
# if [ "$WARNING_COUNT" -gt 0 ]; then
#     echo "=== Warnings ($WARNING_COUNT) ==="
#     grep -A 2 "caught WARNING" /tmp/mud-compile-detail.log | head -30
#     echo
# fi

# Show runtime errors if any
if [ -f "$LOG_FILE" ]; then
    RUNTIME_ERROR_COUNT=$(grep -c "Unhandled.*ERROR" "$LOG_FILE" 2>/dev/null)
    GREP_STATUS=$?
    if [ $GREP_STATUS -ne 0 ]; then
        RUNTIME_ERROR_COUNT=0
    fi
else
    RUNTIME_ERROR_COUNT=0
fi

if [ "$RUNTIME_ERROR_COUNT" -gt 0 ]; then
    echo "=== Runtime Errors ($RUNTIME_ERROR_COUNT) ==="
    grep -A 5 "Unhandled.*ERROR" "$LOG_FILE"
    echo
fi

# Summary
echo "=== Summary ==="
echo "Compilation errors: $ERROR_COUNT"
# echo "Warnings: $WARNING_COUNT"
echo "Runtime errors: $RUNTIME_ERROR_COUNT"
echo
echo "Full log: $LOG_FILE"
echo "View with: less /tmp/mud-compile-detail.log"

# Clean up temporary file
rm -f /tmp/mud-compile-check.lisp

# Exit with error if we found any errors
if [ "$ERROR_COUNT" -gt 0 ] || [ "$RUNTIME_ERROR_COUNT" -gt 0 ]; then
    exit 1
else
    exit 0
fi
