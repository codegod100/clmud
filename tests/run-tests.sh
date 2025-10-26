#!/bin/bash

# CLMUD Test Runner Script
# Provides convenient commands for running different test suites

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

cd "$SCRIPT_DIR"

usage() {
    echo "Usage: $0 [command]"
    echo ""
    echo "Commands:"
    echo "  all       Run all test suites"
    echo "  unit      Run unit tests only"
    echo "  commands  Run command tests only"
    echo "  integration Run integration tests only"
    echo "  fiveam    Run FiveAM test suite"
    echo "  help      Show this help message"
    echo ""
    echo "Examples:"
    echo "  $0 all"
    echo "  $0 unit"
    echo "  $0 commands"
}

run_all_tests() {
    echo "Running all CLMUD tests..."
    sbcl --script run-all-tests.lisp
}

run_unit_tests() {
    echo "Running unit tests..."
    for test in unit/test-*.lisp; do
        if [ -f "$test" ]; then
            echo "Running $test..."
            sbcl --script "$test"
        fi
    done
}

run_command_tests() {
    echo "Running command tests..."
    for test in commands/test-*.lisp; do
        if [ -f "$test" ]; then
            echo "Running $test..."
            sbcl --script "$test"
        fi
    done
}

run_integration_tests() {
    echo "Running integration tests..."
    for test in integration/test-*.lisp; do
        if [ -f "$test" ]; then
            echo "Running $test..."
            sbcl --script "$test"
        fi
    done
}

run_fiveam_tests() {
    echo "Running FiveAM test suite..."
    sbcl --script test-runner.lisp
}

case "${1:-all}" in
    all)
        run_all_tests
        ;;
    unit)
        run_unit_tests
        ;;
    commands)
        run_command_tests
        ;;
    integration)
        run_integration_tests
        ;;
    fiveam)
        run_fiveam_tests
        ;;
    help|--help|-h)
        usage
        ;;
    *)
        echo "Unknown command: $1"
        usage
        exit 1
        ;;
esac
