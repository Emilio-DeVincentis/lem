# Lem Spring Auto-Fix

This extension provides an agentic function to automatically fix Spring Boot compilation errors.

## Features

- **Automatic build and fix:** The `spring-auto-fix` command will automatically run the build, and if it fails, it will send the stack trace to an LLM to get a fix.
- **Asynchronous workflow:** The entire process is non-blocking, using Lem's timers to manage the build-fail-fix-rebuild cycle.
- **Retry limit:** The agent will stop after a configurable number of retries to prevent infinite loops.

## Commands

- `M-x spring-auto-fix`: Start the Spring auto-fix agent.

## Loading the Extension

To load this extension, add the following to your `init.lisp` file:

```lisp
(ql:quickload :lem-spring-auto-fix)
```
