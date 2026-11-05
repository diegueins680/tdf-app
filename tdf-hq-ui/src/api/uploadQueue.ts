interface WaitingUpload { start: () => void; cancel: () => void }

/** Bound file transfers across widgets; queued cancellations never start I/O. */
export class UploadQueue {
  private active = 0;
  private waiting: WaitingUpload[] = [];
  constructor(private readonly capacity: number) {
    if (!Number.isSafeInteger(capacity) || capacity < 1) throw new Error('Invalid upload concurrency');
  }
  private wake(): void {
    while (this.active < this.capacity && this.waiting.length > 0) this.waiting.shift()?.start();
  }
  private enter(signal: AbortSignal): Promise<void> {
    return new Promise((resolve, reject) => {
      const aborted = () => new DOMException('Upload cancelled', 'AbortError');
      if (signal.aborted) { reject(aborted()); return; }
      const entry: WaitingUpload = {
        start: () => {
          signal.removeEventListener('abort', entry.cancel);
          if (signal.aborted) { reject(aborted()); return; }
          this.active += 1;
          resolve();
        },
        cancel: () => {
          signal.removeEventListener('abort', entry.cancel);
          this.waiting = this.waiting.filter((waiting) => waiting !== entry);
          reject(aborted());
        },
      };
      signal.addEventListener('abort', entry.cancel, { once: true });
      if (this.active < this.capacity) entry.start(); else this.waiting.push(entry);
    });
  }
  async run<T>(task: () => Promise<T>, signal: AbortSignal): Promise<T> {
    await this.enter(signal);
    try {
      if (signal.aborted) throw new DOMException('Upload cancelled', 'AbortError');
      return await task();
    } finally { this.active -= 1; this.wake(); }
  }
}

export const sharedFileUploadQueue = new UploadQueue(3);
