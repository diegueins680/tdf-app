// TAX-FILTER-001 "near me" default, kept apart from the surface hook so
// overlays (the radio) do not depend on the session or the fan profile.
import { useEffect, useRef } from 'react';
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
  const attempted = useRef(false);
  useEffect(() => {
    if (attempted.current || !options || isSharedTaxonomyInitialized()) return;
    if (!session) return;
    attempted.current = true;
    let cancelled = false;
    Fans.getProfile()
      .then((profile) => {
        if (cancelled || isSharedTaxonomyInitialized()) return;
        const city = matchCityByName(options.cities, profile.fpCity);
        if (city) setSharedTaxonomySelection({ ...emptyTaxonomySelection(), cityIds: [city.id] });
        else markSharedTaxonomyInitialized();
      })
      .catch(() => {
        if (!cancelled) markSharedTaxonomyInitialized();
      });
    return () => {
      cancelled = true;
    };
  }, [options, session]);
};
