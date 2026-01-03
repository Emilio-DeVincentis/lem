# Lem AI Chat

This extension provides a simple AI chat interface for Lem.

## Features

- **Split-screen interface:** The chat interface is split into a history buffer and a prompt buffer.
- **Markdown rendering:** The chat history is rendered as Markdown, including syntax highlighting for code blocks.
- **Diff preview:** You can preview the changes in a code block before applying them.
- **Apply changes:** You can apply the changes from a code block to the target buffer.

## Commands

- `M-x ai-chat`: Create a new AI chat buffer.
- `Return` (in prompt buffer): Send the message in the prompt buffer to the chat history.
- `C-c C-r` (in history buffer): Simulate a response from the AI.
- `C-c C-d` (in history buffer): Show a diff of the last code block.
- `C-c C-a` (in history buffer): Apply the last code block to the target buffer.

## Loading the Extension

To load this extension, add the following to your `init.lisp` file:

```lisp
(ql:quickload :lem-ai-chat)
```
