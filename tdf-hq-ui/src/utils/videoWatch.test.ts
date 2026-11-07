import { VideoWatch } from './videoWatch';
it('counts visible real playback, excluding seeks, backwards jumps and background gaps', () => {
  const watch = new VideoWatch();
  watch.start(0, 0);
  expect(watch.sample(1, 1000, true)).toBe(1);
  expect(watch.sample(100, 1100, true)).toBe(1);
  expect(watch.sample(101, 2100, false)).toBe(1);
  expect(watch.sample(102, 12100, true)).toBe(1);
  expect(watch.sample(103, 13100, true)).toBe(2);
  watch.stop(); watch.start(20, 20000);
  expect(watch.sample(21, 21000, true)).toBe(3);
  expect(watch.sample(19, 22000, true)).toBe(3);
});
