import { Alert, Box, Button, Chip, Stack, Typography } from '@mui/material';
import { useMemo, useState } from 'react';
import type { DirectorySearchItem } from '../../api/directory';

const DEFAULT_CENTER = { latitude: -0.180653, longitude: -78.467834 };

export default function OpenStreetMapResults({ items }: { items: DirectorySearchItem[] }) {
  const mappable = useMemo(
    () => items.filter((item) => Number.isFinite(item.location.latitude) && Number.isFinite(item.location.longitude)),
    [items],
  );
  const [selectedId, setSelectedId] = useState<string | null>(null);
  const selected = mappable.find((item) => item.id === selectedId) ?? mappable[0];
  const center = selected?.location.latitude != null && selected.location.longitude != null
    ? { latitude: selected.location.latitude, longitude: selected.location.longitude }
    : DEFAULT_CENTER;
  const delta = 0.12;
  const bbox = [center.longitude - delta, center.latitude - delta, center.longitude + delta, center.latitude + delta]
    .map((value) => value.toFixed(6))
    .join('%2C');
  const marker = `${center.latitude.toFixed(6)}%2C${center.longitude.toFixed(6)}`;
  const mapUrl = `https://www.openstreetmap.org/export/embed.html?bbox=${bbox}&layer=mapnik&marker=${marker}`;

  return (
    <Stack spacing={2} aria-label="Resultados en mapa">
      <Alert severity="info">
        El mapa usa ubicaciones aproximadas autorizadas. TDF no muestra direcciones residenciales ni coordenadas privadas.
      </Alert>
      <Box
        component="iframe"
        title="Mapa OpenStreetMap de resultados"
        src={mapUrl}
        loading="lazy"
        referrerPolicy="no-referrer"
        sx={{ width: '100%', minHeight: { xs: 360, md: 520 }, border: 0, borderRadius: 3 }}
      />
      {mappable.length > 0 ? (
        <Stack direction="row" gap={1} flexWrap="wrap" aria-label="Marcadores del mapa">
          {mappable.map((item) => (
            <Chip
              key={`${item.type}:${item.id}`}
              label={`${item.title}${item.location.city ? ` · ${item.location.city}` : ''}`}
              color={item.id === selected?.id ? 'primary' : 'default'}
              onClick={() => setSelectedId(item.id)}
              clickable
            />
          ))}
        </Stack>
      ) : (
        <Typography color="text.secondary">Estos resultados no tienen una ubicación pública aproximada.</Typography>
      )}
      <Button
        component="a"
        href={`https://www.openstreetmap.org/?mlat=${center.latitude}&mlon=${center.longitude}#map=12/${center.latitude}/${center.longitude}`}
        target="_blank"
        rel="noreferrer"
        sx={{ alignSelf: 'flex-start' }}
      >
        Abrir en OpenStreetMap
      </Button>
    </Stack>
  );
}
