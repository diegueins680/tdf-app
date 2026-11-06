import { jest } from '@jest/globals';
import { cleanup, fireEvent, render, screen } from '@testing-library/react';
import '@testing-library/jest-dom';

const capture = jest.fn();
jest.unstable_mockModule('../analytics/posthog', () => ({ getAnalyticsClient: () => ({ capture }) }));
const { default: SessionVideoPreview } = await import('./SessionVideoPreview');
afterEach(() => { cleanup(); capture.mockClear(); });

it('records observable public file playback without exposing media URLs', () => {
  render(<SessionVideoPreview url="https://media.example.com/session.mp4" title="Session" trackPublicPlayback />);
  const video = screen.getByLabelText('Session');
  fireEvent.play(video); fireEvent.ended(video);
  expect(capture.mock.calls.map((row) => row[0])).toEqual(['video_campaign_play_started', 'video_campaign_play_ended']);
  expect(JSON.stringify(capture.mock.calls)).not.toContain('media.example.com');
  fireEvent.error(video);
  expect(screen.getByRole('alert')).toHaveTextContent('No se pudo reproducir');
  expect(screen.getByRole('link')).toHaveAttribute('href', 'https://media.example.com/session.mp4');
});

it('does not count private editor previews or provider frame loads as playback', () => {
  const { rerender } = render(<SessionVideoPreview url="https://media.example.com/session.mp4" title="Preview" />);
  fireEvent.play(screen.getByLabelText('Preview'));
  rerender(<SessionVideoPreview url="https://youtu.be/msDxB2NU3c4" title="Provider" trackPublicPlayback />);
  fireEvent.load(screen.getByTitle('Provider'));
  expect(capture).not.toHaveBeenCalled();
});
