require 'time'

def benchmark(descriptor, reps)
  unless descriptor.start_with?("DISABLED")
    puts "running #{descriptor}"

    total = 0

    (1..reps).each do |i|
      now = Time.now

      yield i

      total += (Time.now - now).to_f
    end

    puts("#{descriptor} took #{total / REPS} seconds")
  end
end

REPS = 1_000_000

x = 0

benchmark('DISABLED simple math', REPS) do |i|
  x += i + i * i / i % i
end

def simple_function
end

benchmark('function calls', REPS) do |_i|
  simple_function()
end
