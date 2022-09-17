ss_time = ARGV[0].to_f
ruby_time = ARGV[1].to_f

if ss_time > ruby_time
  puts "ruby was #{ss_time / ruby_time} times faster"
else
  puts "ss was #{ruby_time / ss_time} times faster"
end
