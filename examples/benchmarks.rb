require 'time'

REPS = 1000

x = 0

total = 0

(1..REPS).each do |i|
  now = Time.now

  x += i + i * i / i % i

  total += (Time.now - now).to_f
end

puts("took #{total / REPS}")
