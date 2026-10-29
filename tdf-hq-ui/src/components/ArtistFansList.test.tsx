import '@testing-library/jest-dom';
import { jest } from '@jest/globals';
import { cleanup, fireEvent, render, screen } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { MemoryRouter, Route, Routes } from 'react-router-dom';

jest.unstable_mockModule('../api/fans', () => ({ Fans: { getArtistFans: async () => ({
  items: [{ afFanId: 248, afDisplayName: 'Juan Diego Illescas', afAvatarUrl: '/juan.jpg', afFollowedAt: '2026-10-15T12:00:00Z' }], total: 1,
}) } }));
jest.unstable_mockModule('react-i18next', () => ({ useTranslation: () => ({ t: (key: string) => key }) }));
const { default: ArtistFansList } = await import('./ArtistFansList');
let client: QueryClient;
afterEach(() => { cleanup(); client.clear(); });

it.each(['name', 'picture'])('takes Juan Diego to his user profile when clicking his %s in the artist fan list', async (target) => {
  client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
  render(<MemoryRouter initialEntries={['/artista/337']}><QueryClientProvider client={client}><Routes>
    <Route path="/artista/:artistId" element={<ArtistFansList artistId={337} />} />
    <Route path="/perfil/248" element={<h1>Perfil de Juan Diego</h1>} />
  </Routes></QueryClientProvider></MemoryRouter>);
  const link = await screen.findByRole('link', { name: 'Juan Diego Illescas' });
  expect(link).toHaveAttribute('href', '/perfil/248');
  fireEvent.click(target === 'picture' ? screen.getByRole('img', { name: 'Juan Diego Illescas' }) : screen.getByText('Juan Diego Illescas'));
  expect(screen.getByRole('heading', { name: 'Perfil de Juan Diego' })).toBeInTheDocument();
});
