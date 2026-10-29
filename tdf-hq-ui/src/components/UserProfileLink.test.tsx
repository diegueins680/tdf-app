import '@testing-library/jest-dom';
import { cleanup, fireEvent, render, screen } from '@testing-library/react';
import { afterEach, expect, it } from '@jest/globals';
import { MemoryRouter, Route, Routes } from 'react-router-dom';
import UserProfileLink from './UserProfileLink';

afterEach(cleanup);

it.each(['name', 'picture'])('opens the user profile by clicking the %s', (target) => {
  render(<MemoryRouter><Routes><Route path="/" element={<UserProfileLink partyId={7}>
    <img src="/juan.jpg" alt="Juan Diego Illescas" /><span>Juan Diego Illescas</span>
  </UserProfileLink>} /><Route path="/perfil/:partyId" element={<h1>Perfil de Juan</h1>} /></Routes></MemoryRouter>);
  fireEvent.click(target === 'picture' ? screen.getByRole('img') : screen.getByText('Juan Diego Illescas'));
  expect(screen.getByRole('heading', { name: 'Perfil de Juan' })).toBeInTheDocument();
});

it.each([null, undefined, 0, -1, 'undefined', '../7', '9007199254740992'])('does not invent a profile for an invalid or unavailable identity: %s', (partyId) => {
  render(<UserProfileLink partyId={partyId}>Usuario</UserProfileLink>);
  expect(screen.queryByRole('link')).toBeNull();
});
