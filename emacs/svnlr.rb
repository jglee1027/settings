#!/usr/bin/ruby
require 'time'

id = nil
start_date = nil
end_date = nil
cur_arg = nil
ARGV.each do |arg|
  case cur_arg
  when :id
    id = arg
  when :start_date
    start_date = Time.parse(arg)
  when :end_date
    end_date = Time.parse(arg) + (60 * 60 * 24)
  end
  
  cur_arg = nil
  
  case arg
  when "-id"
    cur_arg = :id
  when "-sd"
    cur_arg = :start_date
  when "-ed"
    cur_arg = :end_date
  end
end

if id == nil or start_date == nil or end_date == nil
  puts "Subversion log reporting"
  puts "Usage: -id <user-account> -sd <start-date> -ed <end-date>"
  exit
end

puts "========================================================================"
puts "  #{start_date} ~ #{end_date}"

header_regex = /^r\d+ \| \w+ \| .+ \| \d+ lines?$/
header_regex_kr = /^r\d+ \| \w+ \| .+ \| \d+ 개의 행$/
is_continue_puts = true
while line = STDIN.gets
  if line =~ header_regex || line =~ header_regex_kr
    header = line.split(" | ")
    header_id = header[1]
    header_date = Time.parse(header[2].sub(/\(.+\)/,""))
    if header_id == id && header_date >= start_date && header_date <= end_date
      is_continue_puts = true
    else
      is_continue_puts = false
    end
  end
  puts line if is_continue_puts
end
