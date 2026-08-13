import { useQuery } from '@tanstack/react-query';
import {
  Alert,
  Card,
  CardActionArea,
  CardContent,
  Chip,
  CircularProgress,
  Grid,
  Stack,
  Typography,
} from '@mui/material';
import { Link as RouterLink } from 'react-router-dom';
import { Catalogs, type CatalogDefinition } from '../api/catalogs';
import PageShell from '../components/PageShell';

export const catalogEditorPath = (entityKind: string): string | undefined => {
  switch (entityKind) {
    case 'radio_auto_stop_option': return '/configuracion/catalogos/radio-auto-stop';
    case 'appearance_mode_option': return '/configuracion/catalogos/apariencia';
    case 'feedback_category':
    case 'feedback_severity': return '/configuracion/catalogos/feedback';
    case 'reaction_type': return '/configuracion/catalogos/reacciones';
    case 'content_reaction_type': return '/configuracion/catalogos/reacciones-contenido';
    default: return undefined;
  }
};

function DefinitionContent({ definition, editable }: {
  definition: CatalogDefinition;
  editable: boolean;
}) {
  return (
    <CardContent>
      <Stack spacing={1}>
        <Stack direction="row" spacing={0.75} flexWrap="wrap" alignItems="center">
          <Typography variant="h6">{definition.name}</Typography>
          <Chip size="small" label={editable ? 'Editor estricto' : 'Consulta'} color={editable ? 'primary' : 'default'} />
          {definition.sensitive && <Chip size="small" label="Sensible" color="warning" />}
        </Stack>
        <Typography variant="body2" color="text.secondary">
          {definition.description ?? definition.code}
        </Typography>
        <Typography variant="caption" color="text.secondary">
          {definition.code} · {definition.entityKind} · revisión {definition.cacheRevision}
        </Typography>
      </Stack>
    </CardContent>
  );
}

export default function CatalogsPage() {
  const definitionsQuery = useQuery({
    queryKey: ['catalog', 'definitions', 'es'],
    queryFn: () => Catalogs.listDefinitions('es'),
  });
  const definitions = definitionsQuery.data ?? [];

  return (
    <PageShell
      title="Catálogos"
      subtitle="Datos de producto persistidos, versionados y publicados mediante revisión."
    >
      {definitionsQuery.isLoading ? (
        <Stack direction="row" spacing={1} alignItems="center">
          <CircularProgress size={20} />
          <Typography>Cargando definiciones autorizadas…</Typography>
        </Stack>
      ) : definitionsQuery.isError ? (
        <Alert severity="error">
          No se pudieron cargar las definiciones autorizadas. Ningún catálogo se infiere desde el frontend.
        </Alert>
      ) : definitions.length === 0 ? (
        <Alert severity="info">No hay catálogos activos disponibles para esta cuenta.</Alert>
      ) : (
        <Grid container spacing={2}>
          {definitions.map((definition) => {
            const path = catalogEditorPath(definition.entityKind);
            return (
              <Grid item xs={12} md={6} key={definition.id}>
                <Card sx={{ height: '100%' }}>
                  {path ? (
                    <CardActionArea component={RouterLink} to={path} sx={{ height: '100%' }}>
                      <DefinitionContent definition={definition} editable />
                    </CardActionArea>
                  ) : (
                    <DefinitionContent definition={definition} editable={false} />
                  )}
                </Card>
              </Grid>
            );
          })}
        </Grid>
      )}
    </PageShell>
  );
}
