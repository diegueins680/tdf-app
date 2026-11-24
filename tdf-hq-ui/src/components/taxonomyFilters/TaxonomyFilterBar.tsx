// TAX-FILTER-001: shared location and genre filter controls. Every surface
// renders the same bar: removable chips for the active values, a "Clear all"
// action, four multi-selects with per-value counts and a "Near me" shortcut.
import { useMemo, useState } from 'react';
import {
  Alert,
  Autocomplete,
  Box,
  Button,
  Chip,
  Collapse,
  FormControlLabel,
  Stack,
  Switch,
  TextField,
  Typography,
  useMediaQuery,
} from '@mui/material';
import FilterListIcon from '@mui/icons-material/FilterList';
import MyLocationIcon from '@mui/icons-material/MyLocation';
import { useTranslation } from 'react-i18next';
import type { TaxonomyFacets, TaxonomyFilterOptions } from '../../api/taxonomy';
import {
  emptyTaxonomySelection,
  genreOptions,
  locationOptions,
  matchCityByName,
  nearestCity,
  resolveTaxonomyLanguage,
  removeTaxonomyValue,
  setTaxonomyDimension,
  taxonomyChips,
  type TaxonomyDimension,
  type TaxonomyOption,
} from './taxonomyFilterModel';
import type { SurfaceTaxonomyFilters } from './useTaxonomyFilters';

export interface TaxonomyFilterBarProps {
  filters: SurfaceTaxonomyFilters;
  options: TaxonomyFilterOptions | undefined;
  optionsError?: boolean;
  facets?: TaxonomyFacets | null;
  /** Dimensions this surface offers; all four by default. */
  dimensions?: readonly TaxonomyDimension[];
  /** Free-text profile city used by "Near me" before asking for device location. */
  profileCity?: string | null;
  /** Extra line under the controls (e.g. how radio treats cities). */
  note?: string;
  defaultExpanded?: boolean;
  idPrefix: string;
}

const ALL_DIMENSIONS: readonly TaxonomyDimension[] = ['countryIds', 'regionIds', 'cityIds', 'genreIds'];

export default function TaxonomyFilterBar({
  filters,
  options,
  optionsError = false,
  facets,
  dimensions = ALL_DIMENSIONS,
  profileCity,
  note,
  defaultExpanded = false,
  idPrefix,
}: TaxonomyFilterBarProps) {
  const { t, i18n } = useTranslation();
  const language = resolveTaxonomyLanguage(i18n?.resolvedLanguage ?? i18n?.language);
  const reduceMotion = useMediaQuery('(prefers-reduced-motion: reduce)', { noSsr: true });
  const [expanded, setExpanded] = useState(defaultExpanded);
  const [onlyHere, setOnlyHere] = useState(false);
  const [nearMeMessage, setNearMeMessage] = useState<string | null>(null);
  const { selection, isOverride } = filters;

  const chips = useMemo(
    () => taxonomyChips(selection, options, language, t('taxonomyFilters.unknown')),
    [language, options, selection, t],
  );
  const lists = useMemo(() => {
    if (!options) return null;
    return { ...locationOptions(options, selection, language, facets), genres: genreOptions(options.genres, language, facets) };
  }, [facets, language, options, selection]);

  const update = (dimension: TaxonomyDimension, ids: string[]) =>
    filters.setSelection(setTaxonomyDimension(selection, dimension, ids), onlyHere);

  const selectCity = (cityId: string) => {
    if (!selection.cityIds.includes(cityId)) update('cityIds', [...selection.cityIds, cityId]);
  };

  const nearMe = () => {
    if (!options) return;
    setNearMeMessage(null);
    const fromProfile = matchCityByName(options.cities, profileCity);
    if (fromProfile) {
      selectCity(fromProfile.id);
      return;
    }
    if (typeof navigator === 'undefined' || !navigator.geolocation) {
      setNearMeMessage(t('taxonomyFilters.nearMeUnavailable'));
      return;
    }
    setNearMeMessage(t('taxonomyFilters.nearMeLocating'));
    navigator.geolocation.getCurrentPosition(
      (position) => {
        const city = nearestCity(options.cities, position.coords.latitude, position.coords.longitude);
        if (city) {
          setNearMeMessage(null);
          selectCity(city.id);
        } else {
          setNearMeMessage(t('taxonomyFilters.nearMeUnavailable'));
        }
      },
      (error) => setNearMeMessage(t(error.code === error.PERMISSION_DENIED ? 'taxonomyFilters.nearMeDenied' : 'taxonomyFilters.nearMeUnavailable')),
      { maximumAge: 10 * 60_000, timeout: 10_000 },
    );
  };

  const labelFor: Record<TaxonomyDimension, string> = {
    countryIds: t('taxonomyFilters.country'),
    regionIds: t('taxonomyFilters.region'),
    cityIds: t('taxonomyFilters.city'),
    genreIds: t('taxonomyFilters.genre'),
  };
  const listFor = (dimension: TaxonomyDimension): TaxonomyOption[] => {
    if (!lists) return [];
    if (dimension === 'countryIds') return lists.countries;
    if (dimension === 'regionIds') return lists.regions;
    if (dimension === 'cityIds') return lists.cities;
    return lists.genres;
  };
  const optionText = (option: TaxonomyOption) =>
    option.count === undefined ? option.label : `${option.label} ${t('taxonomyFilters.countSuffix', { count: option.count })}`;

  // Every active value stays visible and removable, even for a dimension this
  // surface has no control for (it still narrows the results).
  const visibleChips = chips;
  const panelId = `${idPrefix}-taxonomy-panel`;

  return (
    <Box component="section" aria-label={t('taxonomyFilters.title')} data-testid={`${idPrefix}-taxonomy-filters`}>
      <Stack direction="row" spacing={1} useFlexGap flexWrap="wrap" alignItems="center">
        <Button
          size="small"
          variant={expanded ? 'contained' : 'outlined'}
          startIcon={<FilterListIcon />}
          aria-expanded={expanded}
          aria-controls={panelId}
          onClick={() => setExpanded((value) => !value)}
        >
          {visibleChips.length > 0
            ? t('taxonomyFilters.activeCount', { count: visibleChips.length })
            : t('taxonomyFilters.open')}
        </Button>
        {visibleChips.map((chip) => (
          <Chip
            key={`${chip.dimension}:${chip.id}`}
            label={chip.label}
            size="small"
            // The chip is announced as "Remove …", so the whole chip removes it, not only the icon.
            onClick={() => filters.setSelection(removeTaxonomyValue(selection, chip.dimension, chip.id), onlyHere)}
            onDelete={() => filters.setSelection(removeTaxonomyValue(selection, chip.dimension, chip.id), onlyHere)}
            aria-label={t('taxonomyFilters.remove', { label: `${labelFor[chip.dimension]}: ${chip.label}` })}
            data-testid={`${idPrefix}-taxonomy-chip`}
          />
        ))}
        {visibleChips.length > 0 && (
          <Button size="small" onClick={() => filters.setSelection(emptyTaxonomySelection(), onlyHere)}>{t('taxonomyFilters.clearAll')}</Button>
        )}
      </Stack>

      {isOverride && (
        <Alert
          severity="info"
          sx={{ mt: 1 }}
          action={(
            <Stack direction={{ xs: 'column', sm: 'row' }} spacing={0.5}>
              <Button size="small" color="inherit" onClick={filters.applyEverywhere}>{t('taxonomyFilters.applyEverywhere')}</Button>
              <Button size="small" color="inherit" onClick={filters.resetToShared}>{t('taxonomyFilters.useShared')}</Button>
            </Stack>
          )}
        >
          {t('taxonomyFilters.overrideNotice')}
        </Alert>
      )}

      <Collapse in={expanded} id={panelId} timeout={reduceMotion ? 0 : undefined} unmountOnExit>
        <Stack spacing={1.5} sx={{ mt: 1.5 }}>
          {optionsError && <Alert severity="warning">{t('taxonomyFilters.loadError')}</Alert>}
          <Box sx={{ display: 'grid', gap: 1.5, gridTemplateColumns: { xs: '1fr', sm: 'repeat(2, minmax(0, 1fr))', lg: `repeat(${dimensions.length}, minmax(0, 1fr))` } }}>
            {dimensions.map((dimension) => {
              const list = listFor(dimension);
              const byId = new Map(list.map((option) => [option.id, option]));
              const value = selection[dimension].map((id) => byId.get(id) ?? { id, label: chips.find((chip) => chip.id === id)?.label ?? t('taxonomyFilters.unknown'), depth: 0 });
              return (
                <Autocomplete<TaxonomyOption, true, false, false>
                  key={dimension}
                  multiple
                  size="small"
                  id={`${idPrefix}-taxonomy-${dimension}`}
                  options={list}
                  value={value}
                  loading={!options && !optionsError}
                  disableCloseOnSelect
                  filterSelectedOptions={false}
                  isOptionEqualToValue={(option, selected) => option.id === selected.id}
                  getOptionLabel={(option) => option.label}
                  noOptionsText={t('taxonomyFilters.noMatches')}
                  onChange={(_event, next) => update(dimension, next.map((option) => option.id))}
                  renderTags={() => null}
                  renderOption={(props, option, { selected }) => {
                    const { key, ...rest } = props as typeof props & { key: string };
                    return (
                      <li key={key} {...rest}>
                        <Box sx={{ pl: option.depth * 2, display: 'flex', flexDirection: 'column', opacity: option.count === 0 && !selected ? 0.6 : 1 }}>
                          <Typography variant="body2" component="span">{optionText(option)}</Typography>
                          {option.context && <Typography variant="caption" color="text.secondary" component="span">{option.context}</Typography>}
                        </Box>
                      </li>
                    );
                  }}
                  renderInput={(params) => (
                    <TextField
                      {...params}
                      label={labelFor[dimension]}
                      placeholder={selection[dimension].length ? undefined : t('taxonomyFilters.anyValue')}
                    />
                  )}
                />
              );
            })}
          </Box>
          <Stack direction={{ xs: 'column', sm: 'row' }} spacing={1} alignItems={{ sm: 'center' }} justifyContent="space-between">
            {dimensions.includes('cityIds') ? (
              <Button size="small" variant="text" startIcon={<MyLocationIcon />} onClick={nearMe} disabled={!options}>
                {t('taxonomyFilters.nearMe')}
              </Button>
            ) : <span />}
            <FormControlLabel
              control={<Switch size="small" checked={onlyHere || isOverride} onChange={(event) => setOnlyHere(event.target.checked)} disabled={isOverride} />}
              label={<Typography variant="body2">{t('taxonomyFilters.onlyHere')}</Typography>}
              title={t('taxonomyFilters.onlyHereHelp')}
            />
          </Stack>
          {nearMeMessage && <Typography variant="body2" role="status" color="text.secondary">{nearMeMessage}</Typography>}
          {note && <Typography variant="caption" color="text.secondary">{note}</Typography>}
        </Stack>
      </Collapse>
    </Box>
  );
}
