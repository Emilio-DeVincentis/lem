# Lem Context Manager

This extension provides a context management system for AI agents in Lem. It gathers relevant information from the editor and formats it into a structured prompt for an LLM.

## Features

- **Cursor Position:** The current file, line, and column.
- **Recent Files:** The last 5 visited files.
- **LSP Diagnostics:** The current LSP diagnostics for the buffer.
- **Java Class Structure:** The class structure of the current Java buffer.

## Commands

- `M-x context-manager-display-context`: Gathers all context information and displays it in a new buffer.

## Loading the Extension

To load this extension, add the following to your `init.lisp` file:

```lisp
(ql:quickload :lem-context-manager)
```
