#!/usr/bin/env ruby

require 'time'
require 'ostruct'

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
  unless descriptor.start_with?("DISABLED")
    puts "running #{descriptor}"

    timer = Timer.new

    (1..reps).each do |i|
      yield timer, i
    end

    elapsed = timer.elapsed

    puts("#{descriptor} took #{elapsed} seconds, or #{elapsed / reps} per exec")
  end
end

REPS = 10_000_000

x = 0

benchmark('DISABLED_simple math', REPS) do |timer, i|
  i += 1
  x += i + i * i / i % i
end

def simple_function
end

benchmark('DISABLED_function calls', REPS) do |timer, _i|
  simple_function()
end

$OBJ = OpenStruct.new(foobarbaz: "foobarbaz")

benchmark("global & member access", REPS) do |timer, _|
  timer.start
  $OBJ.foobarbaz
  timer.stop
end
