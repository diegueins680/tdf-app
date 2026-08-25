import {
  MAX_TORRENT_FILE_BYTES,
  isMagnetLink,
  isTorrentFile,
  magnetDisplayName,
  pickPlayableAudioFile,
} from './torrentAudio';

describe('torrentAudio', () => {
  it('recognizes BitTorrent magnets and extracts their display name', () => {
    const magnet = 'magnet:?xt=urn:btih:0123456789abcdef0123456789abcdef01234567&dn=Album%20TDF';

    expect(isMagnetLink(magnet)).toBe(true);
    expect(magnetDisplayName(magnet)).toBe('Album TDF');
    expect(
      isMagnetLink('magnet:?dn=Album&xt=urn:btih:0123456789abcdef0123456789abcdef01234567'),
    ).toBe(true);
    expect(isMagnetLink('https://example.com/audio.mp3')).toBe(false);
    expect(isMagnetLink('magnet:?dn=missing-hash')).toBe(false);
  });

  it('accepts bounded .torrent metadata files and rejects unrelated or oversized files', () => {
    expect(isTorrentFile({ name: 'album.torrent', size: 2048, type: '' })).toBe(true);
    expect(isTorrentFile({ name: 'album.bin', size: 2048, type: 'application/x-bittorrent' })).toBe(true);
    expect(isTorrentFile({ name: 'album.mp3', size: 2048, type: 'audio/mpeg' })).toBe(false);
    expect(
      isTorrentFile({
        name: 'oversized.torrent',
        size: MAX_TORRENT_FILE_BYTES + 1,
        type: 'application/x-bittorrent',
      }),
    ).toBe(false);
  });

  it('chooses the largest playable audio file and ignores non-audio files', () => {
    const files = [
      { name: 'cover.jpg', length: 50_000, type: 'image/jpeg' },
      { name: 'intro.mp3', length: 1_000, type: 'audio/mpeg' },
      { name: 'main.FLAC', length: 8_000, type: 'application/octet-stream' },
      { name: 'notes.txt', length: 20_000, type: 'text/plain' },
    ];

    expect(pickPlayableAudioFile(files)).toEqual(files[2]);
    expect(pickPlayableAudioFile([{ name: 'cover.jpg', length: 50_000 }])).toBeNull();
  });
});
