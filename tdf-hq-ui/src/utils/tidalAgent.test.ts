import { extractTidalCode, generateTidalCode } from './tidalAgent';

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

describe('generateTidalCode', () => {
  it('selects the techno pattern for club prompts', () => {
    const result = generateTidalCode('Club groove for the night');

    expect(result.code).toContain('sound "bd*4"');
    expect(result.tempo).toBe(132);
  });

  it('selects the ambient pattern when mood is drone', () => {
    const result = generateTidalCode('Slow atmosphere', 'drone');

    expect(result.code).toContain('sound "pad*2 mist"');
    expect(result.tempo).toBe(78);
  });
});
