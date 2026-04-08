export const METADATA_ROUTES = {
  base: '/metadata',
  list: '/metadata',
  detail: (id: string | number) => `/metadata/${id}`,
} as const;
