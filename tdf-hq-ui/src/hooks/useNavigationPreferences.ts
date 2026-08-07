import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';

import { NavigationPreferences, type NavigationPreferenceDTO } from '../api/navigationPreferences';

export type NavigationPreferenceChange = Pick<NavigationPreferenceDTO, 'featureId' | 'favorite' | 'pinned' | 'pinOrder'>;

export function useNavigationPreferences(enabled: boolean) {
  const queryClient = useQueryClient();
  const query = useQuery({
    queryKey: ['navigation-preferences'],
    queryFn: NavigationPreferences.list,
    enabled,
    staleTime: 30_000,
  });
  const update = useMutation({
    mutationFn: ({ featureId, favorite, pinned, pinOrder }: NavigationPreferenceChange) =>
      NavigationPreferences.update(featureId, { favorite, pinned, pinOrder }),
    onSuccess: (preference) => {
      queryClient.setQueryData<NavigationPreferenceDTO[]>(['navigation-preferences'], (current = []) => [
        preference,
        ...current.filter((entry) => entry.featureId !== preference.featureId),
      ]);
    },
  });
  const visit = useMutation({
    mutationFn: NavigationPreferences.visit,
    onSuccess: (preference) => {
      queryClient.setQueryData<NavigationPreferenceDTO[]>(['navigation-preferences'], (current = []) => [
        preference,
        ...current.filter((entry) => entry.featureId !== preference.featureId),
      ]);
    },
  });
  return { query, update, visit };
}
