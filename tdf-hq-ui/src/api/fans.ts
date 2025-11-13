import { get, post, put, del } from './client';
import type {
  ArtistProfileDTO,
  ArtistReleaseDTO,
  FanProfileDTO,
  FanProfileUpdate,
  FanFollowDTO,
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
};
