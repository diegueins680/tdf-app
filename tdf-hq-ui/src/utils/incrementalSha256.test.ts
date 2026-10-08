import { IncrementalSha256, sha256Hex } from './incrementalSha256';

const bytes = (value: string) => new TextEncoder().encode(value);

describe('IncrementalSha256', () => {
  it('matches published SHA-256 vectors', () => {
    expect(sha256Hex(bytes(''))).toBe('e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855');
    expect(sha256Hex(bytes('abc'))).toBe('ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad');
  });

  it('produces the same digest across arbitrary update boundaries', () => {
    const digest = new IncrementalSha256();
    digest.update(bytes('The quick brown '));
    digest.update(bytes('fox jumps over '));
    digest.update(bytes('the lazy dog'));
    expect(digest.hex()).toBe('d7a8fbb307d7809469ca9abcb0082e4f8d5651e46d3cdb762d02d0bf37c9e592');
  });

  it('cannot accept bytes after finalization', () => {
    const digest = new IncrementalSha256().update(bytes('done'));
    digest.hex();
    expect(() => digest.update(bytes('more'))).toThrow('already finalized');
  });
});
