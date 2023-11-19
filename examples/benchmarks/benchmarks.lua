local socket = require('socket');
local posix = require('posix');

local Timer = {};
Timer.__index = Timer;

function Timer.new()
  local timer = {
    timestamp = os.time(),
    elapsed = 0,
  };

  setmetatable(timer, Timer);

  return timer;
end

function Timer:start()
  self.timestamp = socket.gettime();
end

function Timer:stop()
  if self.timestamp ~= nil then
    self.elapsed = self.elapsed + (socket.gettime() - self.timestamp)
    self.timestamp = nil
  end
end

function benchmark(descriptor, reps, b)
  if string.match(descriptor, "DISABLED") then
    return;
  end
  print(string.format("running %s", descriptor));
  local timer = Timer.new();

  for i = 0, reps do
    b(timer, i);
  end

  local elapsed = timer.elapsed;

  print(string.format("%s took %f seconds, or %f per exec", descriptor, elapsed, elapsed / reps))
end

local REPS = 10000000;

X = 0;

benchmark('simple math', REPS, function(timer, i)
  timer:start();
  i = i + 1;
  X = X + i + i * i / i % i;
  timer:stop();
end);

function simple_function()
end

benchmark('function calls', REPS, function(timer, _)
  timer:start();
  simple_function();
  timer:stop();
end);

OBJ = { foobarbaz = nil };

benchmark('global & member access', REPS, function(timer, _)
  timer:start();
  local _ = OBJ.foobarbaz;
  timer:stop();
end)
