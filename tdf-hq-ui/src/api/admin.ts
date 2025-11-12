import { post } from './client';
import type { PartyRole } from './generated/client';

export interface CreateUserPayload {
  partyId: number;
  username?: string | null;
  roles?: PartyRole[];
}

export const Admin = {
  createUser: (payload: CreateUserPayload) =>
    post('/admin/users', {
      uacPartyId: payload.partyId,
      uacUsername: payload.username ?? null,
      uacRoles: payload.roles,
    }),
};
