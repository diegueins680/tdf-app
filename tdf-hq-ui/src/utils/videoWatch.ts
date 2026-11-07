/** Client-reported visible playback estimate; seeking and suspended time are never watch time. */
export class VideoWatch {
  seconds = 0;
  private position: number | null = null;
  private clock = 0;
  start(position: number, now: number) { this.position = position; this.clock = now; }
  stop() { this.position = null; }
  sample(position: number, now: number, visible: boolean) {
    if (this.position !== null && visible) {
      const media = position - this.position;
      const wall = (now - this.clock) / 1000;
      if (media > 0 && media <= 5 && wall >= 0 && wall <= 5) this.seconds += Math.min(media, wall);
    }
    this.position = position; this.clock = now;
    return this.seconds;
  }
}
