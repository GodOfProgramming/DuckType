use std::time::mono;

class Clock {
  self as struct {
    _elapsed: 0,
    timestamp: mono::now(),
  }

  fn restart(self) {
    self.timestamp = mono::now();
  }

  fn stop(self) {
    if self.timestamp {
      self._elapsed += mono::elapsed(self.timestamp);
      self.timestamp = nil;
    }
  }

  fn elapsed(self) {
    ret self._elapsed;
  }
}