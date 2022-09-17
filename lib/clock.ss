use time.Monotonic;

class Clock {
  new(self) {
    self._elapsed = 0;
  }

  fn start(self) {
    self.timestamp = Monotonic.now();
  }

  fn stop(self) {
    if self.timestamp {
      self._elapsed += Monotonic.elapsed(self.timestamp);
      self.timestamp = nil;
    }
  }

  fn elapsed(self) {
    ret self._elapsed;
  }
}