---
name: resolve-issue
description: Resolve a GitHub issue by analyzing it, implementing the fix, running tests, and creating a PR. Accepts an issue number or context description.
disable-model-invocation: true
argument-hint: "[issue number or context description]"
---

# Resolve GitHub Issue

You are resolving a GitHub issue for the current repository. Follow each phase carefully.

## Phase 1: Understand the Issue

Check the argument: `$ARGUMENTS`

### If argument is a number (issue number):

1. Fetch issue details:
   ```bash
   gh issue view <number> --json title,body,labels,comments,assignees
   ```
2. Read through the issue title, body, and comments to fully understand the problem
3. Summarize your understanding to the user

### If argument is text (context description):

1. Search for matching open issues:
   ```bash
   gh issue list --state open --search "<context>" --json number,title,body,labels
   ```
2. If a matching issue is found, use it. If not, inform the user and ask whether to proceed without an issue or create one first
3. Analyze the provided context to understand what needs to be done

## Phase 2: Check for Existing Local Changes

1. Check if the user has already made local changes:
   ```bash
   git status
   git diff --stat
   ```
2. If changes exist related to the issue:
   - Review the changes with `git diff <file>` for relevant files
   - Verify they address the issue requirements
   - If changes are correct, skip to Phase 4 (branch creation) or Phase 5 (commit)
   - If changes need modification, inform the user what needs to be adjusted
3. If unrelated changes are mixed in:
   - Identify which files are relevant to the issue
   - Plan to commit only the relevant files (use `git add <specific-files>` instead of `git add .`)

## Phase 3: Plan the Solution

**Skip this phase if the user has already implemented the fix locally.**

1. Explore the codebase to understand the relevant code:
   - Read CLAUDE.md or README.md if present for project conventions
   - Find and read the files related to the issue
   - Understand existing patterns and architecture
2. Present a concise implementation plan to the user
3. Ask the user for confirmation before proceeding:
   - Options: "Proceed", "Modify plan", "Cancel"

## Phase 4: Implement the Fix

1. Check the repository's branch naming convention:
   ```bash
   git branch -r | head -20
   ```
   - Follow the existing convention (e.g., `fix/`, `feat/`, `issue/`, `feature/`)
   - If no clear convention exists, use: `fix/` for bugs, `feat/` for features, `docs/` for documentation
2. Create a new branch from the current branch:
   ```bash
   git checkout -b <prefix>/<issue-number>-<short-description>
   ```
3. If changes are not yet implemented, implement them following the plan
4. Keep changes minimal and focused on the issue

## Phase 5: Test

1. Check if the project has a test command defined in CLAUDE.md, package.json, pyproject.toml, or Makefile
2. Run the project's recommended test suite
3. Report test results to the user:
   - If all tests pass: show a summary
   - If tests fail: analyze failures, attempt to fix, and re-run
4. If no test configuration is found, inform the user and ask how to verify

## Phase 6: Review and Commit

**IMPORTANT: Always ask for user confirmation before committing.**

1. Show the user a summary of all changes:
   ```bash
   git diff --stat
   git diff
   ```
2. Draft a commit message following conventional format:
   ```
   type: short description

   Detailed explanation of changes.

   Closes #<issue-number>
   ```
3. Ask the user for confirmation:
   - Options: "Commit and create PR", "Review changes", "Modify changes", "Cancel"
4. Only after user approval:
   ```bash
   git add <specific-files>
   git commit -m "<message>"
   ```

## Phase 7: Create PR

**IMPORTANT: Always ask for user confirmation before pushing and creating PR.**

1. Push the branch:
   ```bash
   git push -u origin <branch-name>
   ```
2. Create a PR with:
   ```bash
   gh pr create --title "<title>" --body "<body>"
   ```
   - PR body must include `Closes #<issue-number>` to auto-close the issue on merge
   - Include a Summary section and a Test plan section
3. Display the created PR URL to the user

## Guidelines

- Always read and understand existing code before making changes
- Follow the project's existing coding style and conventions
- Keep changes focused — do not fix unrelated issues or refactor unrelated code
- Run tests before committing to catch regressions
- Never force push or amend commits without user approval
- If stuck or uncertain, ask the user rather than guessing
