#!/usr/bin/env ruby

require 'pp'

trap("SIGINT"){exit 1}

#-------------------------------------------------------------------------
#- Rules
#-------------------------------------------------------------------------
class Rule
  attr_accessor :column, :regex, :replacement, :regexStr
  def initialize(col, reg, rep)
    @column      = ColumnSpan.new(col)
    @regex       = Regexp.compile(reg)
    @replacement = rep
  end

  def match(col, index)
    return false unless @column == index
    col =~ @regex
  end

  def gsub(str)
    #puts "#{str} -->  #{str.gsub(@regex, @replacement)}"
    #str.gsub(@regex, @replacement)
    @regex =~ str
    eval @replacement
  end
end

class ColumnSpan
  include Comparable
  attr :span

  def initialize(i)
    @span = i
  end
  def <=>(other)
    case other.class.to_s
      when "Fixnum" then
        if @span == "*" then 0 else @span.to_i <=> other end
      when "ColumnSpan" then
        if @span == "*" then 
          if other.span == "*" then 0 else -1 end
        else
          if other.span == "*" then 1 else @span.to_i <=> other.span.to_i end
        end
      else
        puts other.class.to_s
        raise ArgumentError
    end
  end
end


#-------------------------------------------------------------------------
#- Parse Helpers
#-------------------------------------------------------------------------
class ParseError < RuntimeError
  def initialize(msg)
    @message = msg
  end
  def message
    "Parse Error: #{@message}"
  end
end

RuleRegex = Regexp.compile(
"^([[:digit:]]+(,[[:digit:]]+)*|[*])[[:space:]]+/(.*)/[[:space:]]+[{](.*)[}]$"
)

#-------------------------------------------------------------------------
#- Script Class
#-------------------------------------------------------------------------
class Script
  attr :rules
  
  def initialize(fileName, outh = $stdout, outs = ' ')
    @rules = Script.parse(fileName)
    @outh  = outh
    @outs  = outs
  end

  #----------------------------------------------------------------
  #- Running
  #----------------------------------------------------------------
  def run(handle)
    handle.each_line do|line|
      out = []
      cols = line.split
      cols.each_with_index {|col,index| out << applyRules(col, index)}
      @outh.puts out.join(@outs)
    end
  end

  def applyRules(col,index)
    rule =
      @rules.find {|r|
        r.match(col,index + 1)
      }
    return col if rule == nil
    rule.gsub(col)
  end

  #----------------------------------------------------------------
  #- Parsing
  #----------------------------------------------------------------
  def self.parse(fileName)
    rules = []
    File.open(fileName, "r").each do |line|
      line.chomp!
      next if line == "" || line =~ /^#/
      if line =~ RuleRegex then
        begin
          rules << Rule.new($1, $3, $4)
        rescue RegexError
          raise ParseError.new("Bad regex: -->#{$3}<--")
        end
      else
        raise ParseError.new("Bad line: -->#{line}<--")
      end
    end
    rules
  end
end

#-------------------------------------------------------------------------
#- Main
#-------------------------------------------------------------------------
if __FILE__ == $0 then
  begin
    script = Script.new(ARGV.first)
    script.run($stdin)
  rescue ParseError => e
    puts e.message
    exit 1
  end
end

