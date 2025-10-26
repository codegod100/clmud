# CLMUD Test Suite

This directory contains the comprehensive test suite for the CLMUD (Common Lisp MUD) project.

## Directory Structure

```
tests/
├── README.md                    # This file
├── run-all-tests.lisp          # Main test runner - executes all tests
├── test-runner.lisp            # FiveAM-based test runner for unit tests
├── unit/                       # Unit tests for individual components
│   ├── test-command-registration.lisp
│   ├── test-commands-load.lisp
│   └── test-macro-expansion.lisp
├── commands/                   # Command-specific tests
│   ├── test-get-command-compilation.lisp
│   ├── test-get-command.lisp
│   ├── test-get-step-by-step.lisp
│   └── test-minimal-get.lisp
└── integration/                # Integration tests
    ├── test-get-with-add-inventory.lisp
    ├── test-get-with-announce.lisp
    ├── test-get-with-loot.lisp
    ├── test-get-with-quest-rewards.lisp
    ├── test-get-with-remove-item.lisp
    └── test-get-with-write-crlf.lisp
```

## Running Tests

### Run All Tests
```bash
cd tests
sbcl --script run-all-tests.lisp
```

### Run Individual Test Suites
```bash
# Unit tests only
sbcl --script unit/test-command-registration.lisp

# Command tests only  
sbcl --script commands/test-get-command.lisp

# Integration tests only
sbcl --script integration/test-get-with-loot.lisp
```

### Run FiveAM Tests
```bash
cd tests
sbcl --script test-runner.lisp
```

## Test Categories

### Unit Tests (`unit/`)
- Test individual components in isolation
- Focus on specific functions and macros
- Fast execution, minimal dependencies

### Command Tests (`commands/`)
- Test command parsing and execution
- Verify command registration and dispatch
- Test command-specific logic

### Integration Tests (`integration/`)
- Test complete workflows and interactions
- Verify system behavior with multiple components
- Test real-world usage scenarios

## Test Development Guidelines

1. **File Naming**: Use `test-` prefix followed by descriptive name
2. **Structure**: Each test file should be executable standalone
3. **Dependencies**: Load required MUD components in proper order
4. **Output**: Use clear pass/fail indicators (✓/✗)
5. **Error Handling**: Use `handler-case` for graceful error reporting

## Adding New Tests

1. Create test file in appropriate subdirectory
2. Follow existing naming conventions
3. Include proper error handling
4. Update `run-all-tests.lisp` to include new test
5. Test the new test file individually before adding to suite

## Test Dependencies

Tests require the following to be loaded in order:
1. `src/packages.lisp`
2. `src/ansi.lisp`
3. `src/player.lisp`
4. `src/inventory.lisp`
5. `src/merchant.lisp`
6. `src/world.lisp`
7. `src/mob.lisp`
8. `src/combat.lisp`
9. `src/quest.lisp`
10. `src/server/core.lisp`

## Integration with Development Workflow

The test suite integrates with the development workflow:
- Run `./dev.sh test` to execute all tests
- Tests are automatically validated during development
- Failed tests prevent deployment
- Test results are logged for debugging
