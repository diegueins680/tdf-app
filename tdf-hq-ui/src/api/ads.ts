import { get, post } from './client';

export interface AdsInquiryPayload {
  name?: string;
  email?: string;
  phone?: string;
  course?: string;
  message?: string;
  channel?: string;
}

export interface AdsInquiryDTO {
  inquiryId: number;
  createdAt: string;
  name?: string | null;
  email?: string | null;
  phone?: string | null;
  course?: string | null;
  message?: string | null;
  channel?: string | null;
  status: string;
}

export interface AdsInquiryOut {
  ok: boolean;
  inquiryId: number;
  partyId: number;
  repliedVia: string[];
}

export const Ads = {
  submit: (payload: AdsInquiryPayload) => post<AdsInquiryOut>('/ads/inquiry', payload),
  list: () => get<AdsInquiryDTO[]>('/ads/inquiries'),
};
