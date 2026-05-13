import { useState } from 'react';
import { useParams, Link as RouterLink } from 'react-router-dom';
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import {
  Box, Button, Card, CardContent, Chip, Divider, IconButton, Stack, TextField, Typography,
  Avatar, Grid, ImageList, ImageListItem, Dialog, DialogActions, DialogContent, DialogTitle,
} from '@mui/material';
import {
  ArrowBack as ArrowBackIcon,
  PhotoLibrary as PhotoLibraryIcon,
  Edit as EditIcon,
  Save as SaveIcon,
  Cancel as CancelIcon,
} from '@mui/icons-material';
import PageShell, { EmptyState, SkeletonCards } from '../components/PageShell';
import { Fans } from '../api/fans';
import { useSession } from '../session/SessionContext';
import type { FanClubMemberProfileDTO, FanClubMemoryDTO } from '../api/types';

export default function FanClubMemberProfilePage() {
  const { artistId, partyId } = useParams<{ artistId: string; partyId: string }>();
  const artistIdNum = parseInt(artistId || '0', 10);
  const partyIdNum = parseInt(partyId || '0', 10);
  const { session } = useSession();
  const qc = useQueryClient();
  const [editing, setEditing] = useState(false);
  const [editForm, setEditForm] = useState({ handle: '', bio: '', avatarUrl: '' });

  const clubQuery = useQuery({
    queryKey: ['fan-club', artistIdNum],
    queryFn: () => Fans.getMyClub(artistIdNum),
    enabled: artistIdNum > 0,
  });

  const profilesQuery = useQuery({
    queryKey: ['fan-club-member-profiles', artistIdNum],
    queryFn: () => Fans.listClubMemberProfiles(artistIdNum),
    enabled: artistIdNum > 0,
  });

  const myProfileQuery = useQuery({
    queryKey: ['fan-club-my-member-profile', artistIdNum],
    queryFn: () => Fans.getMyClubMemberProfile(artistIdNum),
    enabled: artistIdNum > 0,
  });

  const memoriesQuery = useQuery({
    queryKey: ['fan-club-memories', artistIdNum],
    queryFn: () => Fans.listClubMemories(artistIdNum),
    enabled: artistIdNum > 0,
  });

  const isMe = session?.partyId === partyIdNum;
  const profile = profilesQuery.data?.find(p => p.fcmpPartyId === partyIdNum);
  const myProfile = myProfileQuery.data;

  const updateProfile = useMutation({
    mutationFn: () => Fans.updateMyClubMemberProfile(artistIdNum, {
      fcmpuHandle: editForm.handle || null,
      fcmpuBio: editForm.bio || null,
      fcmpuAvatarUrl: editForm.avatarUrl || null,
    }),
    onSuccess: () => {
      qc.invalidateQueries({ queryKey: ['fan-club-member-profiles', artistIdNum] });
      qc.invalidateQueries({ queryKey: ['fan-club-my-member-profile', artistIdNum] });
      setEditing(false);
    },
  });

  const memberMemories = memoriesQuery.data?.filter(m => m.fcmMemberProfileId === profile?.fcmpId && !m.fcmIsHidden && !m.fcmIsDeleted) ?? [];

  if (clubQuery.isLoading || profilesQuery.isLoading) {
    return <SkeletonCards count={3} />;
  }

  if (!clubQuery.data) {
    return (
      <EmptyState
        icon={<PhotoLibraryIcon fontSize="large" />}
        title="Club no encontrado"
        description="Este artista aún no tiene un club de fans activo."
      />
    );
  }

  if (!profile) {
    return (
      <EmptyState
        icon={<PhotoLibraryIcon fontSize="large" />}
        title="Miembro no encontrado"
        description="Este miembro no pertenece al club de fans."
      />
    );
  }

  return (
    <PageShell
      title={profile.fcmpDisplayName}
      subtitle={profile.fcmpHandle || `@miembro-${profile.fcmpPartyId}`}
      loading={false}
    >
      <Stack spacing={3}>
        <Button
          component={RouterLink}
          to={`/fans/clubs/${artistIdNum}`}
          startIcon={<ArrowBackIcon />}
          sx={{ alignSelf: 'flex-start' }}
        >
          Volver al club
        </Button>

        <Card>
          <CardContent>
            <Stack spacing={2}>
              <Stack direction="row" spacing={2} alignItems="center">
                <Avatar src={profile.fcmpAvatarUrl || undefined} sx={{ width: 80, height: 80 }} />
                <Box flexGrow={1}>
                  <Typography variant="h5" fontWeight={600}>{profile.fcmpDisplayName}</Typography>
                  {profile.fcmpHandle && (
                    <Typography variant="subtitle1" color="text.secondary">@{profile.fcmpHandle}</Typography>
                  )}
                  <Chip size="small" label={`Miembro desde ${new Date(profile.fcmpJoinedAt).toLocaleDateString()}`} />
                </Box>
                {isMe && (
                  <Button
                    variant="outlined"
                    startIcon={editing ? <CancelIcon /> : <EditIcon />}
                    onClick={() => {
                      if (editing) {
                        setEditing(false);
                      } else {
                        setEditForm({
                          handle: myProfile?.fcmpHandle || '',
                          bio: myProfile?.fcmpBio || '',
                          avatarUrl: myProfile?.fcmpAvatarUrl || '',
                        });
                        setEditing(true);
                      }
                    }}
                  >
                    {editing ? 'Cancelar' : 'Editar perfil'}
                  </Button>
                )}
              </Stack>

              {editing ? (
                <Stack spacing={2}>
                  <TextField
                    label="Handle"
                    value={editForm.handle}
                    onChange={e => setEditForm(f => ({ ...f, handle: e.target.value }))}
                    fullWidth
                    placeholder="@tuhandle"
                  />
                  <TextField
                    label="Bio"
                    value={editForm.bio}
                    onChange={e => setEditForm(f => ({ ...f, bio: e.target.value }))}
                    multiline
                    rows={3}
                    fullWidth
                  />
                  <TextField
                    label="Avatar URL"
                    value={editForm.avatarUrl}
                    onChange={e => setEditForm(f => ({ ...f, avatarUrl: e.target.value }))}
                    fullWidth
                    placeholder="https://..."
                  />
                  <Button
                    variant="contained"
                    startIcon={<SaveIcon />}
                    onClick={() => updateProfile.mutate()}
                    disabled={updateProfile.isPending}
                  >
                    Guardar
                  </Button>
                </Stack>
              ) : (
                <>
                  {profile.fcmpBio && (
                    <Typography variant="body1">{profile.fcmpBio}</Typography>
                  )}
                </>
              )}
            </Stack>
          </CardContent>
        </Card>

        <Typography variant="h6" fontWeight={600}>Recuerdos</Typography>
        {memberMemories.length === 0 ? (
          <EmptyState
            icon={<PhotoLibraryIcon fontSize="large" />}
            title="Sin recuerdos"
            description="Este miembro aún no ha compartido recuerdos."
          />
        ) : (
          <Grid container spacing={2}>
            {memberMemories.map(memory => (
              <Grid item xs={12} md={6} key={memory.fcmId}>
                <Card>
                  <CardContent>
                    <Stack spacing={1}>
                      <Typography variant="h6">{memory.fcmTitle}</Typography>
                      {memory.fcmDescription && (
                        <Typography variant="body2" color="text.secondary">{memory.fcmDescription}</Typography>
                      )}
                      {memory.fcmMediaUrls.length > 0 && (
                        <ImageList cols={2} gap={8}>
                          {memory.fcmMediaUrls.map((url, idx) => (
                            <ImageListItem key={idx}>
                              <img src={url} alt={`Memory ${idx}`} loading="lazy" style={{ maxHeight: 200, objectFit: 'cover' }} />
                            </ImageListItem>
                          ))}
                        </ImageList>
                      )}
                      <Typography variant="caption" color="text.secondary">
                        {new Date(memory.fcmCreatedAt).toLocaleString()}
                      </Typography>
                    </Stack>
                  </CardContent>
                </Card>
              </Grid>
            ))}
          </Grid>
        )}
      </Stack>
    </PageShell>
  );
}
