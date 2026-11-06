import { UploadQueue } from './uploadQueue';

it('bounds concurrent effects across separate batches and continues after each completion', async () => {
  const queue = new UploadQueue(2), release: (() => void)[] = [];
  let running = 0, maximum = 0;
  const tasks = Array.from({ length: 6 }, () => queue.run(async () => {
    running += 1; maximum = Math.max(maximum, running);
    await new Promise<void>((resolve) => release.push(resolve));
    running -= 1;
  }, new AbortController().signal));
  await Promise.resolve(); await Promise.resolve();
  expect(release).toHaveLength(2);
  for (let index = 0; index < tasks.length; index += 1) {
    release[index]?.(); await Promise.resolve(); await Promise.resolve(); await Promise.resolve();
  }
  await Promise.all(tasks);
  expect(maximum).toBe(2);
});
it('removes an aborted waiter without executing it or blocking the next upload', async () => {
  const queue = new UploadQueue(1);
  let finish: (() => void) | undefined, effects = 0;
  const first = queue.run(() => new Promise<void>((resolve) => { finish = resolve; }), new AbortController().signal);
  const controller = new AbortController();
  const cancelled = queue.run(async () => { effects += 1; }, controller.signal).catch((error: unknown) => error);
  controller.abort();
  await expect(cancelled).resolves.toMatchObject({ name: 'AbortError' });
  finish?.(); await first;
  await expect(queue.run(async () => 7, new AbortController().signal)).resolves.toBe(7);
  expect(effects).toBe(0);
});
