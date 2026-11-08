import { jest } from '@jest/globals';
import { cleanup, fireEvent, render, screen } from '@testing-library/react';
import { MemoryRouter, Route, Routes } from 'react-router-dom';

let authenticated = false;
jest.unstable_mockModule('../session/SessionContext', () => ({
  useSession: () => ({ session: authenticated ? { username: 'tester' } : null }),
}));

const { default: NotFoundPage } = await import('./NotFoundPage');

afterEach(() => { cleanup(); authenticated = false; });

function mount(initial = '/missing-page') {
  render(
    <MemoryRouter initialEntries={[initial]}>
      <Routes>
        <Route path="/inicio" element={<p>Public welcome</p>} />
        <Route path="/buscar" element={<p>Public search</p>} />
        <Route path="*" element={<NotFoundPage />} />
      </Routes>
    </MemoryRouter>,
  );
}

test('anonymous visitors can recover to public welcome without forced login', () => {
  mount();
  expect(screen.getByRole('heading', { name: 'Página no encontrada' })).toBeTruthy();
  fireEvent.click(screen.getByRole('button', { name: 'Explorar TDF Records' }));
  expect(screen.getByText('Public welcome')).toBeTruthy();
});

test('anonymous visitors can search from a missing route', () => {
  mount();
  fireEvent.click(screen.getByRole('button', { name: 'Buscar artistas y servicios' }));
  expect(screen.getByText('Public search')).toBeTruthy();
});

test('signed-in visitors retain the public recovery path', () => {
  authenticated = true;
  mount();
  fireEvent.click(screen.getByRole('button', { name: 'Explorar TDF Records' }));
  expect(screen.getByText('Public welcome')).toBeTruthy();
});
