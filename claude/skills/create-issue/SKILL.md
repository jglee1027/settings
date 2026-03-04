---
name: create-issue
description: Create a GitHub issue. Analyzes git diff automatically if no arguments provided, or uses the given context to create an issue.
disable-model-invocation: true
argument-hint: "[optional: issue context or description]"
allowed-tools: Bash, Read, Glob, Grep, AskUserQuestion
---

# Create GitHub Issue

You are creating a GitHub issue for the current repository.

## Determine Mode

Check if arguments were provided: `$ARGUMENTS`

### If no arguments provided (auto-analyze mode):

1. Run `git diff` and `git status` to analyze current uncommitted changes
2. Run `git diff --cached` to check staged changes
3. Run `git log --oneline -5` to understand recent commit context
4. Based on the analysis, determine:
   - Issue type: `feat`, `fix`, `refactor`, `docs`, `chore`, etc.
   - A concise title summarizing the change
   - A detailed body describing the problem and proposed solution

### If arguments provided (context-based mode):

1. Use the provided arguments as context/description for the issue
2. Optionally read relevant files or run `git diff` if the context references code changes
3. Craft a clear title and detailed body based on the provided context

## Create the Issue

1. Before creating, show the user the draft title and body using a summary message
2. Ask the user for confirmation using AskUserQuestion with options: "Create as-is", "Edit title", "Edit body", "Cancel"
3. If confirmed, run:
   ```
   gh issue create --title "<title>" --body "<body>"
   ```
4. Display the created issue URL to the user

## Guidelines

- Title should follow conventional format: `type: short description` (e.g., `feat: add server lifecycle management`)
- Body should include relevant sections like `## Problem`, `## Proposed Solution`, or `## Changes` as appropriate
- Keep the title under 72 characters
- Use markdown formatting in the body
- Do NOT add labels or assignees unless the user explicitly requests them
