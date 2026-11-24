import { API_BASE_URL } from '../api/client';

// Merch images are stored as API-relative paths; make them absolute against the
// API base. Absolute https URLs pass through unchanged.
export const resolveMerchImageUrl = (value?: string | null) => {
  if (!value) return undefined;
  if (/^https:\/\//i.test(value)) return value;
  const base = API_BASE_URL.replace(/\/$/, '');
  return `${base}/${value.replace(/^\//, '')}`;
};
