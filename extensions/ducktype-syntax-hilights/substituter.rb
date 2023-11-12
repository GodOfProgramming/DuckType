#!/usr/bin/env ruby

require 'json'

def replace_reg(word)
  /\${\s*(#{word})\s*}/.freeze
end

REPLACE_REG = replace_reg('\w+')

input = ARGV[0]
output = ARGV[1]

syntax = JSON.parse(File.read(input), symbolize_names: true)

env = syntax.delete(:env)

unless env.nil?
  def sub(env, obj)
    case obj
    when Hash
      for item in obj.values
        sub(env, item)
      end
    when Array
      for item in obj
        sub(env, item)
      end
    when String
      if matches = obj.scan(REPLACE_REG).flatten
        for match in matches
          if e = env[match.to_sym]
            obj.gsub!(replace_reg(match), e)
          else
            STDERR.puts("could not find '#{match}' in env list")
            exit(1)
          end
        end
      end
    end
  end

  sub(env, syntax)
end

File.write(output, syntax.to_json)

