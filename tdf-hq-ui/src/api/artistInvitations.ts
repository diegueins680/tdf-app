import { get, post } from './client';

export type ArtistInvitationLinkStatus = 'active' | 'redeemed' | 'expired' | 'revoked';

export interface ArtistInvitationLinkDTO {
  id: number;
  inviteeLabel: string;
  campaign: string;
  status: ArtistInvitationLinkStatus;
  createdAt: string;
  expiresAt: string;
  redeemedAt: string | null;
  redeemedByPartyId: number | null;
  redeemedByName: string | null;
  revokedAt: string | null;
}

export interface ArtistInvitationLinkIssued {
  invitation: ArtistInvitationLinkDTO;
  // Returned only once, at issuance. Listings never include it.
  token: string;
}

export interface ArtistInvitationLinkCreate {
  inviteeLabel: string;
  campaign?: string;
  expiresInDays?: number;
}

export const ArtistInvitations = {
  list: () => get<ArtistInvitationLinkDTO[]>('/artist-invitations'),
  create: (payload: ArtistInvitationLinkCreate) =>
    post<ArtistInvitationLinkIssued>('/artist-invitations', payload),
  revoke: (id: number) => post<ArtistInvitationLinkDTO>(`/artist-invitations/${id}/revoke`, {}),
};
