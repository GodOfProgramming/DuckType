def bnmk(timer, i)
  # timer.start
  i += 1
  x += i + i * i / i % i
  # timer.stop
end

puts RubyVM::InstructionSequence.disasm(method(:bnmk))
