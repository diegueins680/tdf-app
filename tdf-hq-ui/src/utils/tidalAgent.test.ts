import { extractTidalCode } from './tidalAgent';

describe('extractTidalCode', () => {
  it('keeps valid whitelisted commands', () => {
    const result = extractTidalCode('d1 $ sound "bd*4"\nxfadeIn 4');

    expect(result).toBe('d1 $ sound "bd*4"\nxfadeIn 4');
  });

  it('rejects commands that only match by partial prefix', () => {
    const result = extractTidalCode('d10 $ sound "bd*4"\nhushLater');

    expect(result).toBeNull();
  });

  it('still accepts exact commands with no suffix', () => {
    const result = extractTidalCode('hush');

    expect(result).toBe('hush');
  });
});
