import { get, post, put, del } from './client';
import type {
  ArtistProfileDTO,
  ArtistReleaseDTO,
  ArtistProfileUpsert,
  FanProfileDTO,
  FanProfileUpdate,
  FanFollowDTO,
  FanClubDTO,
  FanClubPostDTO,
  FanClubEventDTO,
  FanClubElectionDTO,
  FanClubCandidacyDTO,
  FanClubCreatePostReq,
  FanClubCreateEventReq,
  FanClubCreateElectionReq,
  FanClubCreateCandidacyReq,
  FanClubVoteReq,
} from './types';

export const Fans = {
  listArtists: () => get<ArtistProfileDTO[]>('/fans/artists'),
  getArtist: (artistId: number) => get<ArtistProfileDTO>(`/fans/artists/${artistId}`),
  getReleases: (artistId: number) => get<ArtistReleaseDTO[]>(`/fans/artists/${artistId}/releases`),
  getProfile: () => get<FanProfileDTO>('/fans/me/profile'),
  updateProfile: (payload: FanProfileUpdate) => put<FanProfileDTO>('/fans/me/profile', payload),
  listFollows: () => get<FanFollowDTO[]>('/fans/me/follows'),
  follow: (artistId: number) => post<FanFollowDTO>(`/fans/me/follows/${artistId}`, {}),
  unfollow: (artistId: number) => del<void>(`/fans/me/follows/${artistId}`),
  getMyArtistProfile: () => get<ArtistProfileDTO>('/fans/me/artist-profile'),
  updateMyArtistProfile: (payload: ArtistProfileUpsert) =>
    put<ArtistProfileDTO>('/fans/me/artist-profile', payload),

  // Fan Club
  getClub: (artistId: number) => get<FanClubDTO>(`/fans/clubs/${artistId}`),
  getClubEvents: (artistId: number) => get<FanClubEventDTO[]>(`/fans/clubs/${artistId}/events`),
  listMyClubs: () => get<FanClubDTO[]>('/fans/me/clubs'),
  getMyClub: (artistId: number) => get<FanClubDTO>(`/fans/me/clubs/${artistId}`),
  listClubPosts: (artistId: number) => get<FanClubPostDTO[]>(`/fans/me/clubs/${artistId}/posts`),
  createClubPost: (artistId: number, payload: FanClubCreatePostReq) =>
    post<FanClubPostDTO>(`/fans/me/clubs/${artistId}/posts`, payload),
  pinClubPost: (artistId: number, postId: number) =>
    post<void>(`/fans/me/clubs/${artistId}/posts/${postId}/pin`, {}),
  unpinClubPost: (artistId: number, postId: number) =>
    post<void>(`/fans/me/clubs/${artistId}/posts/${postId}/unpin`, {}),
  hideClubPost: (artistId: number, postId: number) =>
    post<void>(`/fans/me/clubs/${artistId}/posts/${postId}/hide`, {}),
  unhideClubPost: (artistId: number, postId: number) =>
    post<void>(`/fans/me/clubs/${artistId}/posts/${postId}/unhide`, {}),
  listClubEvents: (artistId: number) => get<FanClubEventDTO[]>(`/fans/me/clubs/${artistId}/events`),
  createClubEvent: (artistId: number, payload: FanClubCreateEventReq) =>
    post<FanClubEventDTO>(`/fans/me/clubs/${artistId}/events`, payload),
  listClubElections: (artistId: number) => get<FanClubElectionDTO[]>(`/fans/me/clubs/${artistId}/elections`),
  createClubElection: (artistId: number, payload: FanClubCreateElectionReq) =>
    post<FanClubElectionDTO>(`/fans/me/clubs/${artistId}/elections`, payload),
  createCandidacy: (artistId: number, electionId: number, payload: FanClubCreateCandidacyReq) =>
    post<FanClubCandidacyDTO>(`/fans/me/clubs/${artistId}/elections/${electionId}/candidacy`, payload),
  castVote: (artistId: number, electionId: number, payload: FanClubVoteReq) =>
    post<void>(`/fans/me/clubs/${artistId}/elections/${electionId}/vote`, payload),
};
