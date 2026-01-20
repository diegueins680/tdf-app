import { post } from './client';

export interface InstagramOAuthExchangeRequest {
  code: string;
  redirectUri?: string | null;
}

export interface InstagramOAuthPage {
  pageId: string;
  pageName: string;
  instagramUserId?: string | null;
  instagramUsername?: string | null;
}

export interface InstagramMedia {
  id: string;
  caption?: string | null;
  mediaUrl?: string | null;
  permalink?: string | null;
  timestamp?: string | null;
}

export interface InstagramOAuthExchangeResponse {
  userId: string;
  userName?: string | null;
  tokenType: string;
  expiresIn: number;
  pages: InstagramOAuthPage[];
  instagramUserId?: string | null;
  instagramUsername?: string | null;
  media: InstagramMedia[];
}

export const InstagramOAuthAPI = {
  exchange: (payload: InstagramOAuthExchangeRequest) =>
    post<InstagramOAuthExchangeResponse>('/instagram/oauth/exchange', payload),
};
