import type { ChangeEvent, Dispatch, RefObject, SetStateAction } from 'react';
import { Avatar, Box, Button, IconButton, Stack, TextField } from '@mui/material';
import EditIcon from '@mui/icons-material/Edit';
import type { FanProfileUpdate } from '../../api/types';
import { ProfileSectionCard } from './ProfileSectionCard';

export function FanProfileEditor({
  avatarInputRef,
  displayNameFallback,
  profileDraft,
  saving,
  setProfileDraft,
  onAvatarFileChange,
  onSave,
}: {
  avatarInputRef: RefObject<HTMLInputElement>;
  displayNameFallback?: string | null;
  profileDraft: FanProfileUpdate;
  saving: boolean;
  setProfileDraft: Dispatch<SetStateAction<FanProfileUpdate>>;
  onAvatarFileChange: (event: ChangeEvent<HTMLInputElement>) => void;
  onSave: () => void;
}) {
  return (
    <ProfileSectionCard
      title="Tu perfil fan"
      description="Personaliza cómo te ve la comunidad cuando sigues a un artista."
      actions={
        <Button
          variant="contained"
          sx={{ alignSelf: 'flex-start' }}
          onClick={onSave}
          disabled={saving}
        >
          {saving ? 'Guardando…' : 'Guardar perfil'}
        </Button>
      }
    >
      <Stack direction={{ xs: 'column', md: 'row' }} spacing={3} alignItems="flex-start">
        <Stack spacing={1} alignItems="center" sx={{ minWidth: 140 }}>
          <Box sx={{ position: 'relative', width: 104, height: 104 }}>
            <Avatar
              src={profileDraft.fpuAvatarUrl ?? undefined}
              alt={profileDraft.fpuDisplayName ?? displayNameFallback ?? ''}
              sx={{ width: 104, height: 104 }}
            />
            <IconButton
              size="small"
              onClick={() => avatarInputRef.current?.click()}
              sx={{
                position: 'absolute',
                bottom: -8,
                right: -8,
                bgcolor: 'primary.main',
                color: '#fff',
                boxShadow: 2,
                '&:hover': { bgcolor: 'primary.dark' },
              }}
              aria-label="Editar avatar"
            >
              <EditIcon fontSize="small" />
            </IconButton>
            <input
              type="file"
              accept="image/*"
              ref={avatarInputRef}
              onChange={onAvatarFileChange}
              style={{ display: 'none' }}
            />
          </Box>
          <TextField
            label="Avatar (pega enlace o sube)"
            placeholder="https://..."
            value={profileDraft.fpuAvatarUrl ?? ''}
            onChange={(event) => setProfileDraft((prev) => ({ ...prev, fpuAvatarUrl: event.target.value }))}
            fullWidth
            size="small"
          />
          <Stack direction="row" spacing={1}>
            <Button size="small" variant="contained" onClick={() => avatarInputRef.current?.click()}>
              Subir imagen
            </Button>
            {profileDraft.fpuAvatarUrl && (
              <Button
                size="small"
                variant="text"
                onClick={() => setProfileDraft((prev) => ({ ...prev, fpuAvatarUrl: '' }))}
              >
                Quitar
              </Button>
            )}
          </Stack>
        </Stack>
        <Stack flex={1} spacing={2}>
          <Stack direction={{ xs: 'column', md: 'row' }} spacing={2}>
            <TextField
              label="Nombre público"
              value={profileDraft.fpuDisplayName ?? ''}
              onChange={(event) => setProfileDraft((prev) => ({ ...prev, fpuDisplayName: event.target.value }))}
              fullWidth
            />
            <TextField
              label="Ciudad"
              value={profileDraft.fpuCity ?? ''}
              onChange={(event) => setProfileDraft((prev) => ({ ...prev, fpuCity: event.target.value }))}
              fullWidth
            />
          </Stack>
          <Stack direction={{ xs: 'column', md: 'row' }} spacing={2}>
            <TextField
              label="Géneros favoritos"
              value={profileDraft.fpuFavoriteGenres ?? ''}
              onChange={(event) =>
                setProfileDraft((prev) => ({ ...prev, fpuFavoriteGenres: event.target.value }))
              }
              fullWidth
            />
          </Stack>
          <TextField
            label="Bio"
            multiline
            minRows={2}
            value={profileDraft.fpuBio ?? ''}
            onChange={(event) => setProfileDraft((prev) => ({ ...prev, fpuBio: event.target.value }))}
            fullWidth
          />
        </Stack>
      </Stack>
    </ProfileSectionCard>
  );
}
