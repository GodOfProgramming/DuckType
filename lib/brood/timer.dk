use std::time::mono;

export class Timer {
  self as struct {
    elapsed: 0,
    timestamp: mono::now(),
  }

  fn start(self) {
    self.timestamp = mono::now();
  }

  fn stop(self) {
    if self.timestamp != nil {
      self.elapsed += mono::elapsed(self.timestamp);
      self.timestamp = nil;
    }
  }
}