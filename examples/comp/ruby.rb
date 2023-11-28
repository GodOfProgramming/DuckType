def fib n
  if n <= 1
    n
  else
    fib(n - 2) + fib(n - 1)
  end
end

puts RubyVM::InstructionSequence.disasm(method(:fib))
