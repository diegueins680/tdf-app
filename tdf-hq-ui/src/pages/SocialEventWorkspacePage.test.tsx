import { jest } from '@jest/globals';
import { cleanup, render, screen } from '@testing-library/react';
import { MemoryRouter, Route, Routes } from 'react-router-dom';

const overview = jest.fn(() => <div>Canonical overview</div>);
const task = jest.fn(({ eventId, activityId }: { eventId: string; activityId: string | null }) =>
  <div>Scoped task {eventId}:{activityId ?? 'invalid'}</div>);
jest.unstable_mockModule('./SocialEventDetailPage', () => ({ default: overview }));
jest.unstable_mockModule('./EventTaskPage', () => ({ default: task }));
jest.unstable_mockModule('react-i18next', () => ({ useTranslation: () => ({ t: (key: string) => key }) }));
const { default: Workspace } = await import('./SocialEventWorkspacePage');
afterEach(() => { cleanup(); overview.mockClear(); task.mockClear(); });

function visit(query = '') {
  return render(<MemoryRouter initialEntries={[`/social/eventos/80${query}`]}>
    <Routes><Route path="/social/eventos/:eventId" element={<Workspace />} /></Routes>
  </MemoryRouter>);
}
test('ordinary event links preserve the existing overview', async () => {
  visit('?city=Quito'); await screen.findByText('Canonical overview');
  expect(task).not.toHaveBeenCalled();
});
test('task links never mount the overview readers', async () => {
  visit('?tarea=8000'); await screen.findByText('Scoped task 80:8000');
  expect(overview).not.toHaveBeenCalled();
});
test.each(['?tarea=', '?tarea=8000&tarea=9000'])(
  'invalid task selector %s never falls back to the overview', async query => {
    visit(query); await screen.findByText(/Scoped task/);
    expect(overview).not.toHaveBeenCalled();
    expect(task.mock.calls.at(-1)?.[0].activityId).toBe(query === '?tarea=' ? '' : null);
  },
);
