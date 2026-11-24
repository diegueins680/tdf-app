// TAX-FILTER-001 "near me" default, kept apart from the surface hook so
// overlays (the radio) do not depend on the session or the fan profile.
import { useEffect } from 'react';
import { useQuery } from '@tanstack/react-query';
import { Fans } from '../../api/fans';
import type { TaxonomyFilterOptions } from '../../api/taxonomy';
import { useOptionalSession } from '../../session/SessionContext';
import { emptyTaxonomySelection, matchCityByName } from './taxonomyFilterModel';
import { isSharedTaxonomyInitialized, markSharedTaxonomyInitialized, setSharedTaxonomySelection } from './taxonomyFilterStore';

/**
 * "Near me" default: once per browser, pre-select the signed-in viewer's
 * profile city. Never overrides a selection the viewer already made, and
 * never asks for device location (that only happens on an explicit tap).
 */
export const useProfileCityDefault = (options: TaxonomyFilterOptions | undefined) => {
  const session = useOptionalSession()?.session ?? null;
  const viewerId = session?.partyId ?? null;
  const profile = useQuery({
    queryKey: ['fan-profile', viewerId],
    queryFn: Fans.getProfile,
    enabled: viewerId !== null && Boolean(options),
    staleTime: 60_000,
    retry: false,
  });
  const cityName = viewerId !== null ? profile.data?.fpCity : undefined;
  useEffect(() => {
    if (viewerId === null || !options || !profile.isSuccess || isSharedTaxonomyInitialized()) return;
    const city = matchCityByName(options.cities, cityName);
    if (city) setSharedTaxonomySelection({ ...emptyTaxonomySelection(), cityIds: [city.id] });
    else markSharedTaxonomyInitialized();
  }, [options, viewerId, profile.isSuccess, cityName]);
  return cityName;
};
