#!/usr/bin/env ruby

require 'time'

def benchmark(descriptor, reps)
  unless descriptor.start_with?("DISABLED")
    puts "running #{descriptor}"

    now = Time.now
    (1..reps).each do |i|
      yield i
    end
    elapsed = (Time.now - now).to_f

    puts("#{descriptor} took #{elapsed} seconds")
  end
end

REPS = 10_000_000

x = 0

benchmark('simple math', REPS) do |i|
  i += 1
  x += i + i * i / i % i
end

def simple_function
end

benchmark('function calls', REPS) do |_i|
  simple_function()
end
