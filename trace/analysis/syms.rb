#!/usr/bin/env ruby
# This program reads the disassembly from objdump to create a symbol
# table for an executable. The symbol table is printed to stdout with
# one line per symbol. It is formatted as follows
#
# <symbol> <startAddress> <endAddress>
#

# exit gracefully if the pipe dies
trap("SIGPIPE"){exit}

# read symbols from exe given on command line
exe     = ARGV[0]
section = ARGV[1] || ""
if exe.nil? || ARGV.count > 1 then
  puts "usage: syms <exe> [section]"
  exit 1
end

# parse output of objdump
IO.popen("objdump -d #{exe} #{section}") do |pipe|
  while not pipe.eof? do
    line = pipe.gets
    if line =~ /^([[:xdigit:]]+)\s+<((\w|@|-)+)>:/ then
      startAddr = $1
      symbol    = $2
    elsif line =~ /^\s+([[:xdigit:]]+):\s/ then
      last = $1
    elsif (line =~ /^\s*$/) && (not (symbol.nil? || startAddr.nil? || last.nil?))
      puts "#{symbol} #{startAddr} #{last}"
      symbol = startAddr = last = nil
    end
  end
end

