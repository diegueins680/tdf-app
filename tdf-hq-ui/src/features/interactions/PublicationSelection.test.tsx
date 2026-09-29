import { jest } from '@jest/globals';
import { cleanup, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { MemoryRouter } from 'react-router-dom';
import { Card } from '@mui/material';
import LazyPaginatedList from '../../components/LazyPaginatedList';
import { PublicationAnchor, usePublicationSelection } from './PublicationSelection';

function Collection({ parameter, ids, loading = false }: { parameter: string; ids: string[]; loading?: boolean }) {
  const selected = usePublicationSelection(parameter, ids, loading);
  return <>{selected.notice}<LazyPaginatedList items={ids} pagination={{ initialRowsPerPage: 5, rowsPerPageOptions: [5], selectedIndex: selected.index, resetKey: selected.requested }}
    renderItems={(items) => items.map((id) => <Card component={PublicationAnchor} selected={selected.requested === id} key={id} data-testid={`item-${id}`}>Publication {id}</Card>)} /></>;
}
afterEach(cleanup);
beforeEach(() => Object.defineProperty(HTMLElement.prototype, 'scrollIntoView', { configurable: true, value: jest.fn() }));
it.each(['recording', 'session', 'release', 'post', 'memory'])('selects and focuses a late %s publication after loading without rendering the entire list', async (parameter) => {
  const collection = (ids: string[], loading: boolean) => <MemoryRouter initialEntries={[`/collection?${parameter}=17`]}><Collection parameter={parameter} ids={ids} loading={loading} /></MemoryRouter>;
  const view = render(collection([], true));
  expect(screen.queryByRole('status')).toBeNull();
  view.rerender(collection(Array.from({ length: 30 }, (_, index) => String(index)), false));
  const target = await screen.findByTestId('item-17');
  await waitFor(() => expect(document.activeElement).toBe(target));
  expect(target.textContent).toBe('Publication 17');
  expect(screen.queryByTestId('item-0')).toBeNull();
  expect(screen.getAllByTestId(/^item-/)).toHaveLength(5);
  expect(HTMLElement.prototype.scrollIntoView).toHaveBeenCalled();
});
it('keeps unavailable links explicit without focusing unrelated publications', () => {
  render(<MemoryRouter initialEntries={['/collection?post=missing']}><Collection parameter="post" ids={['allowed']} /></MemoryRouter>);
  expect(screen.getByRole('status').textContent).toContain('ya no está disponible');
  expect(document.activeElement).not.toBe(screen.getByTestId('item-allowed'));
});
it('leaves pagination under user control after revealing the requested item', async () => {
  const items = Array.from({ length: 30 }, (_, index) => String(index));
  const collection = (ids: string[]) => <MemoryRouter initialEntries={['/collection?post=17']}><Collection parameter="post" ids={ids} /></MemoryRouter>;
  const view = render(collection(items));
  await screen.findByTestId('item-17');
  fireEvent.click(screen.getByRole('button', { name: /next page|página siguiente/i }));
  await screen.findByTestId('item-20');
  view.rerender(collection([...items, '30']));
  expect(screen.queryByTestId('item-17')).toBeNull();
  expect(screen.getByTestId('item-20')).toBeTruthy();
});
