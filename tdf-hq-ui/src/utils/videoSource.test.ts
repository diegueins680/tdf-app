import { resolveVideoSource, videoSourceKey } from './videoSource';

describe('public video URL boundary', () => {
  it('derives a stable public identity without copying an unlisted hash or arbitrary query', () => {
    expect(videoSourceKey('https://youtu.be/uRNyIMLiqU8')).toBe('youtube:uRNyIMLiqU8');
    expect(videoSourceKey('https://vimeo.com/123456789/privatehash')).toBe('vimeo:123456789');
    expect(videoSourceKey('https://www.instagram.com/reel/DfGMOaVDhJ9/')).toBe('instagram:DfGMOaVDhJ9');
    expect(videoSourceKey('https://www.tiktok.com/@tdf.records.label/video/7703720333777325330')).toBe('tiktok:7703720333777325330');
    expect(videoSourceKey('https://media.example.org/real.mp4')).toBeNull();
  });
  it.each([
    ['https://youtu.be/uRNyIMLiqU8', 'youtube', '/embed/uRNyIMLiqU8'],
    ['https://www.youtube.com/shorts/pUtIlM8Bic4', 'youtube', '/embed/pUtIlM8Bic4'],
    ['https://vimeo.com/123456789/privatehash', 'vimeo', 'h=privatehash'],
    ['https://www.instagram.com/reel/DfGMOaVDhJ9/', 'instagram', '/reel/DfGMOaVDhJ9/embed/'],
    ['https://www.tiktok.com/@tdf.records.label/video/7703720333777325330', 'tiktok', '/player/v1/7703720333777325330'],
    ['https://media.example.org/real.mp4', 'file', 'real.mp4'],
    ['https://api.tdfrecords.net/campaign-platform/public/videos/' + 'a'.repeat(32) + '/media', 'file', '/media'],
  ])('accepts public source %s', (url, provider, path) => {
    expect(resolveVideoSource(url)).toMatchObject({ provider });
    expect(resolveVideoSource(url)?.src).toContain(path);
  });
  it.each(['javascript:alert(1)', 'http://youtu.be/uRNyIMLiqU8', 'https://www.youtube.com.evil.org/watch?v=uRNyIMLiqU8', 'https://user:pass@youtube.com/watch?v=uRNyIMLiqU8', 'https://instagram.com/stories/me/123', 'https://www.youtube.com/playlist?list=abc', 'https://127.0.0.1/test.mp4', 'https://localhost/test.mp4', 'https://example.org/frame.html', '<iframe src="evil"/>'])('refuses executable, unknown or restricted sources %s', value => expect(resolveVideoSource(value)).toBeNull());
});
