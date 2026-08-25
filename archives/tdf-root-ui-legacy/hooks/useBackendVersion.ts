import { useQuery } from '@tanstack/react-query';

export type BuildInfo = {
  app: string;
  version: string;
  builtAt?: string;
  commit?: string;
};

export function useBackendVersion(apiBase: string) {
  return useQuery({
    queryKey: ['backend-version', apiBase],
    queryFn: async () => {
      const res = await fetch(`${apiBase.replace(/\/$/, '')}/version`);
      if (!res.ok) throw new Error('Failed to load /version');
      return (await res.json()) as BuildInfo;
    },
  });
}

