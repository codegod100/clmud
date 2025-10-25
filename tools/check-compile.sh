#!/bin/bash
# Detailed SBCL compilation checker
# Shows all compilation errors and warnings

TOOLS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$TOOLS_DIR")"

cd "$PROJECT_DIR"

echo "=== Detailed Compilation Check ==="
echo

# Run SBCL and capture output
timeout 10 sbcl --noinform --non-interactive --load mud.lisp > /tmp/mud-compile-detail.log 2>&1
EXIT_CODE=$?

# Check exit code
if [ $EXIT_CODE -eq 124 ]; then
    echo "✓ Server started successfully (timeout reached)"
    echo
elif [ $EXIT_CODE -eq 0 ]; then
    echo "✓ Server loaded without errors"
    echo
else
    echo "✗ Server failed to load (exit code: $EXIT_CODE)"
    echo
fi

# Count and show errors
ERROR_COUNT=$(grep -c "caught ERROR" /tmp/mud-compile-detail.log 2>/dev/null || echo 0)
if [ "$ERROR_COUNT" -gt 0 ]; then
    echo "=== Compilation Errors ($ERROR_COUNT) ==="
    grep -B 15 "caught ERROR" /tmp/mud-compile-detail.log | grep -v "^--$"
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
RUNTIME_ERROR_COUNT=$(grep "Unhandled.*ERROR" /tmp/mud-compile-detail.log 2>/dev/null | wc -l)
if [ "$RUNTIME_ERROR_COUNT" -gt 0 ]; then
    echo "=== Runtime Errors ($RUNTIME_ERROR_COUNT) ==="
    grep -A 5 "Unhandled.*ERROR" /tmp/mud-compile-detail.log
    echo
fi

# Summary
echo "=== Summary ==="
echo "Compilation errors: $ERROR_COUNT"
# echo "Warnings: $WARNING_COUNT"
echo "Runtime errors: $RUNTIME_ERROR_COUNT"
echo
echo "Full log: /tmp/mud-compile-detail.log"
echo "View with: less /tmp/mud-compile-detail.log"

# Exit with error if we found any errors
if [ "$ERROR_COUNT" -gt 0 ] || [ "$RUNTIME_ERROR_COUNT" -gt 0 ]; then
    exit 1
else
    exit 0
fi
