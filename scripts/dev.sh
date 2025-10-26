#!/bin/bash
# MUD Development Helper Script
# Provides shortcuts for common development tasks

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR/.."

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

show_help() {
    echo -e "${BLUE}MUD Development Helper${NC}"
    echo
    echo "Usage: ./dev.sh <command>"
    echo
    echo "Commands:"
    echo "  start         - Start the MUD server"
    echo "  validate      - Run full validation (balance + compile)"
    echo "  check         - Quick compilation check"
    echo "  balance       - Check parenthesis balance only"
    echo "  list <file>   - List top-level forms in a file"
    echo "  show <file> <index> - Show a specific form"
    echo "  clean         - Remove temporary files"
    echo "  test          - Run a quick connection test"
    echo "  tests         - Run the full test suite"
    echo "  tests-unit    - Run unit tests only"
    echo "  tests-cmd     - Run command tests only"
    echo "  tests-int     - Run integration tests only"
    echo "  help          - Show this help"
    echo
    echo "Examples:"
    echo "  ./dev.sh start"
    echo "  ./dev.sh validate"
    echo "  ./dev.sh list src/server/commands.lisp"
    echo "  ./dev.sh show src/server/commands.lisp 46"
}

cmd_start() {
    echo -e "${GREEN}Starting MUD server...${NC}"
    echo "Press Ctrl+C to stop"
    echo
    sbcl --script mud.lisp
}

cmd_validate() {
    echo -e "${BLUE}Running full validation...${NC}"
    ./tools/validate.sh
}

cmd_check() {
    echo -e "${BLUE}Checking compilation...${NC}"
    ./tools/check-compile.sh
}

cmd_balance() {
    echo -e "${BLUE}Checking parenthesis balance...${NC}"
    for file in src/*.lisp; do
        if python3 tools/lisp-safe-edit.py "$file" > /dev/null 2>&1; then
            echo -e "${GREEN}✓${NC} $file"
        else
            echo -e "${RED}✗${NC} $file"
            python3 tools/lisp-safe-edit.py "$file" 2>&1 | grep "depth"
        fi
    done
}

cmd_list() {
    if [ -z "$1" ]; then
        echo -e "${RED}Error: file required${NC}"
        echo "Usage: ./dev.sh list <file>"
        exit 1
    fi
    sbcl --script tools/sexp-edit.lisp list "$1" 2>&1 | grep -v "^;"
}

cmd_show() {
    if [ -z "$1" ] || [ -z "$2" ]; then
        echo -e "${RED}Error: file and index required${NC}"
        echo "Usage: ./dev.sh show <file> <index>"
        exit 1
    fi
    sbcl --script tools/sexp-edit.lisp show "$1" "$2" 2>&1 | grep -v "^;"
}

cmd_clean() {
    echo -e "${YELLOW}Cleaning temporary files...${NC}"
    rm -f /tmp/mud-*.log
    rm -f /tmp/attack-*.lisp
    rm -f /tmp/ram-*.lisp
    rm -f /tmp/*.lisp
    echo -e "${GREEN}✓${NC} Cleaned"
}

cmd_test() {
    echo -e "${BLUE}Starting server for quick test...${NC}"

    # Start server in background
    timeout 10 sbcl --script mud.lisp > /tmp/mud-test.log 2>&1 &
    SERVER_PID=$!

    # Wait for server to start
    echo "Waiting for server to start..."
    sleep 2

    # Try to connect
    echo -e "${YELLOW}Testing connection...${NC}"
    if echo "quit" | timeout 2 nc localhost 4000 > /tmp/mud-test-output.log 2>&1; then
        echo -e "${GREEN}✓${NC} Server is responding"
        echo
        echo "Server output:"
        head -5 /tmp/mud-test-output.log
    else
        echo -e "${RED}✗${NC} Could not connect to server"
        echo
        echo "Check /tmp/mud-test.log for details"
    fi

    # Cleanup
    kill $SERVER_PID 2>/dev/null || true
    wait $SERVER_PID 2>/dev/null || true
}

cmd_tests() {
    echo -e "${BLUE}Running full test suite...${NC}"
    cd tests
    ./run-tests.sh all
}

cmd_tests_unit() {
    echo -e "${BLUE}Running unit tests...${NC}"
    cd tests
    ./run-tests.sh unit
}

cmd_tests_cmd() {
    echo -e "${BLUE}Running command tests...${NC}"
    cd tests
    ./run-tests.sh commands
}

cmd_tests_int() {
    echo -e "${BLUE}Running integration tests...${NC}"
    cd tests
    ./run-tests.sh integration
}

# Main command dispatcher
case "${1:-help}" in
    start)
        cmd_start
        ;;
    validate)
        cmd_validate
        ;;
    check)
        cmd_check
        ;;
    balance)
        cmd_balance
        ;;
    list)
        cmd_list "$2"
        ;;
    show)
        cmd_show "$2" "$3"
        ;;
    clean)
        cmd_clean
        ;;
    test)
        cmd_test
        ;;
    tests)
        cmd_tests
        ;;
    tests-unit)
        cmd_tests_unit
        ;;
    tests-cmd)
        cmd_tests_cmd
        ;;
    tests-int)
        cmd_tests_int
        ;;
    help|--help|-h)
        show_help
        ;;
    *)
        echo -e "${RED}Error: Unknown command '$1'${NC}"
        echo
        show_help
        exit 1
        ;;
esac
