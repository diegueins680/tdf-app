import { jest } from '@jest/globals';
import '@testing-library/jest-dom';
import { cleanup, fireEvent, render, screen } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { MemoryRouter } from 'react-router-dom';

const post = jest.fn<(path: string, body: unknown, init: RequestInit) => Promise<unknown>>();
const get = jest.fn<() => Promise<unknown>>();
jest.unstable_mockModule('../api/client', () => ({ get, post }));
const { default: Page } = await import('./EventDemoPage');

const show = (route = '/demo-eventos') => render(
  <QueryClientProvider client={new QueryClient()}><MemoryRouter initialEntries={[route]}><Page/></MemoryRouter></QueryClientProvider>
);
beforeEach(() => { get.mockResolvedValue({available:true}); });

const fill = () => {
  fireEvent.change(screen.getByLabelText(/Nombre/), { target:{value:'Lead sintético'} });
  fireEvent.change(screen.getByLabelText(/Correo/), { target:{value:'demo@example.invalid'} });
  fireEvent.change(screen.getByLabelText(/Qué evento/), { target:{value:'Ensayo local'} });
};
const submit = () => fireEvent.click(screen.getByRole('button',{name:'Agenda una demo con TDF'}));
afterEach(() => { cleanup(); post.mockReset(); get.mockReset(); });

it('records campaign attribution and never calls a request an appointment', async ()=>{
  post.mockResolvedValue({status:'requested'});
  show('/demo-eventos?utm_source=tiktok&utm_medium=organic_social&utm_campaign=tdf_demo_eventos_quito&utm_content=publico');
  await screen.findByLabelText(/Nombre/);
  fill(); submit();
  expect(await screen.findByText(/no hay una cita reservada/i)).toBeInTheDocument();
  expect(post).toHaveBeenCalledWith('/public/event-demo-requests', {
    name:'Lead sintético',email:'demo@example.invalid',message:'Evento: Ensayo local',channel:'tiktok-publico',
  }, {headers:{'Idempotency-Key':expect.any(String)}});
});
it('shows an unconfirmed result and retries with the same key', async ()=>{
  post.mockRejectedValueOnce(new Error('network')).mockResolvedValueOnce({status:'requested'});
  show();
  await screen.findByLabelText(/Nombre/);
  fill(); submit();
  expect(await screen.findByText(/no pudimos confirmar/i)).toBeInTheDocument();
  submit();
  expect(await screen.findByText(/solicitud recibida/i)).toBeInTheDocument();
  expect(post.mock.calls[0][2]).toEqual(post.mock.calls[1][2]);
});

it('does not display or submit a form when intake is disabled', async ()=>{
  get.mockRejectedValueOnce(new Error('disabled'));
  show();
  expect(await screen.findByText(/no están disponibles/i)).toBeInTheDocument();
  expect(screen.queryByRole('button',{name:'Agenda una demo con TDF'})).not.toBeInTheDocument();
  expect(screen.queryByLabelText(/Correo/)).not.toBeInTheDocument();
  expect(post).not.toHaveBeenCalled();
});
