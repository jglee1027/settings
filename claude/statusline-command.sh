#!/bin/bash

# Read JSON input from stdin
input=$(cat)

# Extract values from JSON
cwd=$(echo "$input" | jq -r '.workspace.current_dir')
model=$(echo "$input" | jq -r '.model.display_name')
total_in=$(echo "$input" | jq -r '.context_window.total_input_tokens')
total_out=$(echo "$input" | jq -r '.context_window.total_output_tokens')
remaining=$(echo "$input" | jq -r '.context_window.remaining_percentage // empty')
session_id=$(echo "$input" | jq -r '.session_id')

# 5-hour usage tracking
five_hr_file="$HOME/.claude/usage_5hr_${session_id}.json"
if [ ! -f "$five_hr_file" ]; then
    echo "{}" > "$five_hr_file"
fi

# Cleanup entries older than 5 hours
cutoff=$(($(date +%s) - 18000))
jq --arg cutoff "$cutoff" 'to_entries | map(select((.key | tonumber) >= ($cutoff | tonumber))) | from_entries' "$five_hr_file" > "$five_hr_file.tmp" && mv "$five_hr_file.tmp" "$five_hr_file"

# Record current usage
timestamp=$(date +%s)
jq --arg ts "$timestamp" --arg total_in "$total_in" --arg total_out "$total_out" \
    '.[$ts] = {"input": ($total_in | tonumber), "output": ($total_out | tonumber)}' \
    "$five_hr_file" > "$five_hr_file.tmp" && mv "$five_hr_file.tmp" "$five_hr_file"

# Calculate 5-hour totals
five_hr_in=$(jq '[.[] | .input] | add // 0' "$five_hr_file")
five_hr_out=$(jq '[.[] | .output] | add // 0' "$five_hr_file")
five_hr_total=$((five_hr_in + five_hr_out))

# Session total
session_total=$((total_in + total_out))

# 5-hour limit (default 2M tokens for Team plan)
# Pro: 1000000, Max: 5000000, Team: 2000000
FIVE_HR_LIMIT=${CLAUDE_5HR_LIMIT:-2000000}
five_hr_pct=$((five_hr_total * 100 / FIVE_HR_LIMIT))

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
five_hr_formatted=$(format_tokens $five_hr_total)

# 5-hour display with percentage (color based on usage)
if [ "$five_hr_pct" -ge 80 ]; then
    # Red when high usage
    five_hr_display=$(printf '\033[38;5;196m5h: %s (%d%%)\033[0m' "$five_hr_formatted" "$five_hr_pct")
elif [ "$five_hr_pct" -ge 50 ]; then
    # Yellow when medium usage
    five_hr_display=$(printf '\033[38;5;220m5h: %s (%d%%)\033[0m' "$five_hr_formatted" "$five_hr_pct")
else
    # Orange when low usage
    five_hr_display=$(printf '\033[38;5;216m5h: %s (%d%%)\033[0m' "$five_hr_formatted" "$five_hr_pct")
fi

# Build output
# Directory (cyan) | Git branch | Model (purple) | Session (green) | 5hr (orange/yellow/red) | Context
printf '\033[38;5;75m%s\033[0m%s \033[38;5;183m%s\033[0m \033[38;5;150mS: %s\033[0m %s %s\n' \
    "$(basename "$cwd")" \
    "$git_info" \
    "$model" \
    "$session_formatted" \
    "$five_hr_display" \
    "$ctx_display"
