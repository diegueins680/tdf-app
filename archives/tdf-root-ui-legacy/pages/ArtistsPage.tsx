import { useMemo, useState } from 'react';
import {
  Alert,
  Avatar,
  Box,
  Button,
  Card,
  CardContent,
  Chip,
  CircularProgress,
  Dialog,
  DialogActions,
  DialogContent,
  DialogTitle,
  Divider,
  Grid,
  IconButton,
  List,
  ListItem,
  ListItemAvatar,
  ListItemText,
  Paper,
  Stack,
  Table,
  TableBody,
  TableCell,
  TableContainer,
  TableHead,
  TableRow,
  TextField,
  Tooltip,
  Typography,
} from '@mui/material';
import AddCircleOutlineIcon from '@mui/icons-material/AddCircleOutline';
import MusicNoteIcon from '@mui/icons-material/MusicNote';
import PublicIcon from '@mui/icons-material/Public';
import GroupIcon from '@mui/icons-material/Group';
import LaunchIcon from '@mui/icons-material/Launch';
import CloseIcon from '@mui/icons-material/Close';
import { useQuery } from '@tanstack/react-query';
import { Bands } from '../api/bands';
import type { BandDTO, BandMemberDTO } from '../api/types';

function BandMembersList({ members }: { members: BandMemberDTO[] }) {
  return (
    <List dense disablePadding>
      {members.map((m) => (
        <ListItem key={m.bmId} disablePadding sx={{ py: 0.25 }}>
          <ListItemAvatar sx={{ minWidth: 36 }}>
            <Avatar sx={{ width: 28, height: 28, fontSize: 12 }}>
              {m.bmPartyName.charAt(0).toUpperCase()}
            </Avatar>
          </ListItemAvatar>
          <ListItemText
            primary={m.bmPartyName}
            secondary={m.bmRole ?? 'Miembro'}
            primaryTypographyProps={{ variant: 'body2' }}
            secondaryTypographyProps={{ variant: 'caption' }}
          />
        </ListItem>
      ))}
    </List>
  );
}

function BandDetailDialog({
  band,
  open,
  onClose,
}: {
  band: BandDTO | null;
  open: boolean;
  onClose: () => void;
}) {
  if (!band) return null;

  return (
    <Dialog open={open} onClose={onClose} maxWidth="md" fullWidth>
      <DialogTitle>
        <Stack direction="row" alignItems="center" justifyContent="space-between">
          <span>{band.bName}</span>
          <IconButton onClick={onClose} size="small">
            <CloseIcon />
          </IconButton>
        </Stack>
      </DialogTitle>
      <DialogContent dividers>
        <Grid container spacing={3}>
          <Grid item xs={12} md={4}>
            <Paper variant="outlined" sx={{ p: 2, height: '100%' }}>
              <Stack spacing={2}>
                {band.bPhotoUrl ? (
                  <Box
                    component="img"
                    src={band.bPhotoUrl}
                    alt={band.bName}
                    sx={{ width: '100%', borderRadius: 1, objectFit: 'cover', maxHeight: 240 }}
                  />
                ) : (
                  <Box
                    sx={{
                      width: '100%',
                      height: 200,
                      bgcolor: 'grey.100',
                      borderRadius: 1,
                      display: 'flex',
                      alignItems: 'center',
                      justifyContent: 'center',
                    }}
                  >
                    <MusicNoteIcon sx={{ fontSize: 64, color: 'grey.400' }} />
                  </Box>
                )}
                <Stack direction="row" spacing={1} flexWrap="wrap">
                  {band.bLabelArtist && (
                    <Chip label="Artista del Label" color="primary" size="small" />
                  )}
                  {band.bPrimaryGenre && (
                    <Chip label={band.bPrimaryGenre} variant="outlined" size="small" />
                  )}
                  {band.bHomeCity && (
                    <Chip icon={<PublicIcon fontSize="small" />} label={band.bHomeCity} variant="outlined" size="small" />
                  )}
                </Stack>
              </Stack>
            </Paper>
          </Grid>
          <Grid item xs={12} md={8}>
            <Stack spacing={2}>
              <Paper variant="outlined" sx={{ p: 2 }}>
                <Typography variant="subtitle2" gutterBottom>
                  <GroupIcon fontSize="small" sx={{ verticalAlign: 'middle', mr: 0.5 }} />
                  Integrantes ({band.bMembers.length})
                </Typography>
                <BandMembersList members={band.bMembers} />
              </Paper>

              {band.bContractFlags && (
                <Alert severity="info" sx={{ mt: 1 }}>
                  Contrato: {band.bContractFlags}
                </Alert>
              )}
            </Stack>
          </Grid>
        </Grid>
      </DialogContent>
      <DialogActions>
        <Button onClick={onClose}>Cerrar</Button>
      </DialogActions>
    </Dialog>
  );
}

export default function ArtistsPage() {
  const [search, setSearch] = useState('');
  const [selectedBand, setSelectedBand] = useState<BandDTO | null>(null);

  const bandsQuery = useQuery({
    queryKey: ['bands', 'artists-page'],
    queryFn: async () => {
      const page = await Bands.list({ pageSize: 200 });
      return page.items;
    },
  });

  const filtered = useMemo(() => {
    const items = bandsQuery.data ?? [];
    if (!search.trim()) return items;
    const q = search.toLowerCase();
    return items.filter(
      (b) =>
        b.bName.toLowerCase().includes(q) ||
        (b.bPrimaryGenre ?? '').toLowerCase().includes(q) ||
        (b.bHomeCity ?? '').toLowerCase().includes(q) ||
        b.bMembers.some((m) => m.bmPartyName.toLowerCase().includes(q))
    );
  }, [bandsQuery.data, search]);

  return (
    <Box sx={{ p: 3 }}>
      <Stack spacing={3}>
        <Stack direction="row" alignItems="center" justifyContent="space-between" spacing={2}>
          <Typography variant="h4" component="h1">
            Artistas del Label
          </Typography>
          <Button
            variant="contained"
            startIcon={<AddCircleOutlineIcon />}
            href="/crm/contactos"
          >
            Nuevo artista
          </Button>
        </Stack>

        <TextField
          placeholder="Buscar por nombre, género, ciudad o integrante…"
          value={search}
          onChange={(e: React.ChangeEvent<HTMLInputElement>) => setSearch(e.target.value)}
          fullWidth
        />

        {bandsQuery.isLoading && (
          <Box sx={{ display: 'flex', justifyContent: 'center', py: 6 }}>
            <CircularProgress />
          </Box>
        )}

        {bandsQuery.isError && (
          <Alert severity="error">
            No se pudieron cargar los artistas.{' '}
            {(bandsQuery.error as Error)?.message ?? 'Error desconocido'}
          </Alert>
        )}

        {!bandsQuery.isLoading && filtered.length === 0 && (
          <Paper variant="outlined" sx={{ p: 4, textAlign: 'center' }}>
            <MusicNoteIcon sx={{ fontSize: 48, color: 'grey.400', mb: 1 }} />
            <Typography color="text.secondary">
              {search.trim()
                ? 'No se encontraron artistas con ese criterio.'
                : 'Aún no hay bandas registradas. Crea contactos y bandas desde CRM / Contactos.'}
            </Typography>
          </Paper>
        )}

        <Grid container spacing={2}>
          {filtered.map((band) => (
            <Grid item xs={12} sm={6} lg={4} key={band.bandId}>
              <Card
                variant="outlined"
                sx={{
                  cursor: 'pointer',
                  transition: 'box-shadow 0.2s',
                  '&:hover': { boxShadow: 3 },
                  height: '100%',
                  display: 'flex',
                  flexDirection: 'column',
                }}
                onClick={() => setSelectedBand(band)}
              >
                <CardContent sx={{ flexGrow: 1 }}>
                  <Stack spacing={1.5}>
                    <Stack direction="row" alignItems="center" spacing={1.5}>
                      <Avatar
                        src={band.bPhotoUrl ?? undefined}
                        sx={{ width: 48, height: 48, bgcolor: 'primary.main' }}
                      >
                        {band.bName.charAt(0).toUpperCase()}
                      </Avatar>
                      <Box sx={{ minWidth: 0 }}>
                        <Typography variant="h6" noWrap title={band.bName}>
                          {band.bName}
                        </Typography>
                        <Stack direction="row" spacing={0.5} flexWrap="wrap">
                          {band.bLabelArtist && (
                            <Chip label="Label" color="primary" size="small" />
                          )}
                          {band.bPrimaryGenre && (
                            <Chip label={band.bPrimaryGenre} size="small" variant="outlined" />
                          )}
                        </Stack>
                      </Box>
                    </Stack>

                    <Divider />

                    <Stack direction="row" spacing={1} alignItems="center">
                      <GroupIcon fontSize="small" color="action" />
                      <Typography variant="body2" color="text.secondary">
                        {band.bMembers.length} integrante{band.bMembers.length !== 1 ? 's' : ''}
                      </Typography>
                    </Stack>

                    {band.bHomeCity && (
                      <Stack direction="row" spacing={1} alignItems="center">
                        <PublicIcon fontSize="small" color="action" />
                        <Typography variant="body2" color="text.secondary">
                          {band.bHomeCity}
                        </Typography>
                      </Stack>
                    )}

                    <Box sx={{ mt: 'auto', pt: 1 }}>
                      <Button
                        size="small"
                        endIcon={<LaunchIcon />}
                        onClick={(e: React.MouseEvent<HTMLButtonElement>) => {
                          e.stopPropagation();
                          setSelectedBand(band);
                        }}
                      >
                        Ver detalle
                      </Button>
                    </Box>
                  </Stack>
                </CardContent>
              </Card>
            </Grid>
          ))}
        </Grid>
      </Stack>

      <BandDetailDialog
        band={selectedBand}
        open={!!selectedBand}
        onClose={() => setSelectedBand(null)}
      />
    </Box>
  );
}
