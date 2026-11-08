import { jest } from '@jest/globals';
import { cleanup, fireEvent, render, screen } from '@testing-library/react';
import '@testing-library/jest-dom';

const capture = jest.fn();
jest.unstable_mockModule('../analytics/posthog', () => ({ getAnalyticsClient: () => ({ capture }) }));
const { default: SessionVideoPreview } = await import('./SessionVideoPreview');
afterEach(() => { cleanup(); capture.mockClear(); });

it('records observable public file playback without exposing media URLs', () => {
  render(<SessionVideoPreview url="https://media.example.com/session.mp4" title="Session" trackPublicPlayback />);
  fireEvent.click(screen.getByRole('button', { name: 'Ver video aquí' }));
  const video = screen.getByLabelText('Session');
  fireEvent.play(video); fireEvent.ended(video);
  expect(capture.mock.calls.map((row) => row[0])).toEqual(['video_campaign_play_started', 'video_campaign_play_ended']);
  expect(JSON.stringify(capture.mock.calls)).not.toContain('media.example.com');
  fireEvent.error(video);
  expect(screen.getByRole('alert')).toHaveTextContent('No pudimos reproducir');
  expect(screen.getByRole('link')).toHaveAttribute('href', 'https://media.example.com/session.mp4');
});

it('does not count private editor previews or provider frame loads as playback', () => {
  const { rerender } = render(<SessionVideoPreview url="https://media.example.com/session.mp4" title="Preview" />);
  fireEvent.click(screen.getByRole('button', { name: 'Ver video aquí' }));
  fireEvent.play(screen.getByLabelText('Preview'));
  rerender(<SessionVideoPreview url="https://youtu.be/msDxB2NU3c4" title="Provider" trackPublicPlayback />);
  fireEvent.click(screen.getByRole('button', { name: 'Ver video aquí' }));
  fireEvent.load(screen.getByTitle('Provider'));
  expect(capture).not.toHaveBeenCalled();
});

it.each([
  ['?resourcekey=0-example_key&usp=sharing', '?resourcekey=0-example_key'],
  ['?resourcekey=key%2Bvalue%26other%3Dvalue&usp=sharing', '?resourcekey=key%2Bvalue%26other%3Dvalue'],
  ['?usp=sharing', ''],
  ['?resourcekey=', ''],
])('preserves only a supplied Drive resource key in the preview (%s)', (sourceQuery, previewQuery) => {
  const source = `https://drive.google.com/file/d/synthetic-file/view${sourceQuery}`;
  render(<SessionVideoPreview url={source} title="Drive session" trackPublicPlayback />);
  fireEvent.click(screen.getByRole('button', { name: 'Ver video aquí' }));
  expect(screen.getByTitle('Drive session')).toHaveAttribute('src',
    `https://drive.google.com/file/d/synthetic-file/preview${previewQuery}`);
  expect(screen.getByRole('link')).toHaveAttribute('href', source);
  fireEvent.load(screen.getByTitle('Drive session'));
  expect(capture).not.toHaveBeenCalled();
});
