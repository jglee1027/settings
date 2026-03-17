#!/bin/bash

# Read JSON input from stdin
input=$(cat)

# Extract values from JSON
cwd=$(echo "$input" | jq -r '.workspace.current_dir')
model=$(echo "$input" | jq -r '.model.display_name')
total_in=$(echo "$input" | jq -r '.context_window.total_input_tokens')
total_out=$(echo "$input" | jq -r '.context_window.total_output_tokens')
remaining=$(echo "$input" | jq -r '.context_window.remaining_percentage // empty')

# Session total
session_total=$((total_in + total_out))

# Git information
git_info=""
if cd "$cwd" 2>/dev/null && git rev-parse --git-dir > /dev/null 2>&1; then
    branch=$(git -c core.filemode=false branch --show-current 2>/dev/null || echo 'detached')
    changes=$(git -c core.filemode=false status --porcelain 2>/dev/null | wc -l | tr -d ' ')

    if [ "$changes" -gt 0 ]; then
        # Yellow branch with orange change count
        git_info=$(printf '\033[38;5;220m⎇ %s \033[38;5;208m[%d]\033[0m' "$branch" "$changes")
    else
        # Green branch when clean
        git_info=$(printf '\033[38;5;114m⎇ %s\033[0m' "$branch")
    fi
    git_info=" $git_info"
fi

# Context remaining display
ctx_display=""
if [ -n "$remaining" ]; then
    ctx_pct="${remaining%.*}"
    if [ "$ctx_pct" -lt 20 ]; then
        # Red when low
        ctx_display=$(printf '\033[38;5;196mCtx: %d%%\033[0m' "$ctx_pct")
    elif [ "$ctx_pct" -lt 50 ]; then
        # Yellow when medium
        ctx_display=$(printf '\033[38;5;220mCtx: %d%%\033[0m' "$ctx_pct")
    else
        # Light purple when good
        ctx_display=$(printf '\033[38;5;147mCtx: %d%%\033[0m' "$ctx_pct")
    fi
fi

# Format token counts
format_tokens() {
    local num=$1
    if [ "$num" -ge 1000000 ]; then
        printf "%.1fMt" "$(echo "scale=1; $num / 1000000" | bc)"
    elif [ "$num" -ge 1000 ]; then
        printf "%.0fKt" "$(echo "scale=0; $num / 1000" | bc)"
    else
        printf "%dt" "$num"
    fi
}

session_formatted=$(format_tokens $session_total)

# Build output
# Directory (cyan) | Git branch | Model (purple) | Session (green) | Context
printf '\033[38;5;75m%s\033[0m%s \033[38;5;183m%s\033[0m \033[38;5;150mS: %s\033[0m %s\n' \
    "$(basename "$cwd")" \
    "$git_info" \
    "$model" \
    "$session_formatted" \
    "$ctx_display"
