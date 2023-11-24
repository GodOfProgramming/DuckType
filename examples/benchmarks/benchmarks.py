import time

class Timer:
  def __init__(self) -> None:
    self.elapsed = 0
    self.timestamp = time.time()

  def start(self):
    self.timestamp = time.time()

  def stop(self):
    if self.timestamp != None:
      self.elapsed += (time.time() - self.timestamp)
      self.timestamp = None

def benchmark(descriptor: str, reps: int, f):
  if not descriptor.startswith("DISABLED"):
    print(f"running {descriptor}")

    timer = Timer()

    for i in range(0, reps):
      f(timer, i)

    elapsed = timer.elapsed

    print(f"{descriptor} took {elapsed} seconds, or {elapsed / reps} per exec")

REPS = 10_000_000

x = 0

def simple_math(timer: Timer, i: int):
  timer.start()
  global x
  i += 1
  x += i + i * i / i % i
  timer.stop()

benchmark('simple math', REPS, simple_math)

def simple_function():
  pass

def function_calls(timer: Timer, i: int):
  timer.start()
  simple_function()
  timer.stop()

benchmark('function calls', REPS, function_calls)

class FakeObj:
  def __init__(self) -> None:
    self.foobarbaz = None

OBJ = FakeObj()

def global_member_access(timer: Timer, i: int):
  timer.start()
  global OBJ
  OBJ.foobarbaz
  timer.stop()

benchmark('global & member access', REPS, global_member_access)
