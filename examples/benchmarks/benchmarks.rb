#!/usr/bin/env ruby

require 'time'
require 'ostruct'

DISABLED_PREFIX = 'DISABLED'

class Timer
  attr_accessor :elapsed, :timestamp

  def initialize
    self.elapsed = 0
    self.timestamp = Time.now
  end

  def start
    self.timestamp = Time.now
  end

  def stop
    unless self.timestamp.nil?
      self.elapsed += (Time.now - self.timestamp).to_f
      self.timestamp = nil
    end
  end
end

def benchmark(descriptor, reps)
  unless descriptor.start_with?(DISABLED_PREFIX)
    puts "running #{descriptor}"

    timer = Timer.new

    (1..reps).each do |i|
      yield timer, i
    end

    elapsed = timer.elapsed

    puts("#{descriptor} took #{elapsed} seconds, or #{elapsed / reps} per exec")
  end
end

def simple_function
end

def fib n
  if n <= 1
    n
  else
    fib(n - 2) + fib(n - 1)
  end
end

puts RubyVM::InstructionSequence.disasm(method(:fib))

exit(0)

REPS = 10_000_000

x = 0
benchmark('simple math', REPS) do |timer, i|
  timer.start
  i += 1
  x += i + i * i / i % i
  timer.stop
end

benchmark('function calls', REPS) do |timer, _i|
  timer.start
  simple_function()
  timer.stop
end

$OBJ = OpenStruct.new(foobarbaz: "foobarbaz")

benchmark('global & member access', REPS) do |timer, _|
  timer.start
  $OBJ.foobarbaz
  timer.stop
end

benchmark("fib", 1) do |timer, _|
  timer.start
  fib(39)
  timer.stop
end
