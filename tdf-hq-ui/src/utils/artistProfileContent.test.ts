import { parseArtistJson, parseArtistTextItems } from './artistProfileContent';

describe('parseArtistTextItems', () => {
  it('renders structured artist content as individual items', () => {
    expect(parseArtistTextItems('["Festival TDF","Official collaboration"]')).toEqual([
      'Festival TDF',
      'Official collaboration',
    ]);
  });

  it('preserves legacy plain text and rejects non-string array entries', () => {
    expect(parseArtistTextItems('Legacy achievement')).toEqual(['Legacy achievement']);
    expect(parseArtistTextItems('["Verified",7,null,""]')).toEqual(['Verified']);
    expect(parseArtistTextItems(null)).toEqual([]);
  });

  it('handles malformed JSON without throwing or discarding legacy content', () => {
    expect(parseArtistJson('{"unfinished":')).toBeUndefined();
    expect(parseArtistTextItems('{"unfinished":')).toEqual(['{"unfinished":']);
  });
});
