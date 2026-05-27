import { useState } from 'react';
import { useParams, useNavigate, Link as RouterLink } from 'react-router-dom';
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import {
  Box, Button, Card, CardContent, Chip, Stack, TextField, Typography,
  Avatar, Grid, ImageList, ImageListItem, Dialog, DialogActions, DialogContent, DialogTitle,
} from '@mui/material';
import {
  ArrowBack as ArrowBackIcon,
  PhotoLibrary as PhotoLibraryIcon,
  Edit as EditIcon,
  Save as SaveIcon,
  Cancel as CancelIcon,
  MailOutline as MailIcon,
} from '@mui/icons-material';
import PageShell, { EmptyState, SkeletonCards } from '../components/PageShell';
import LazyPaginatedList from '../components/LazyPaginatedList';
import { Fans } from '../api/fans';
import { ChatAPI } from '../api/chat';
import { useSession } from '../session/SessionContext';
import type { FanClubMemberProfileDTO, FanClubMemoryDTO } from '../api/types';

const FAN_CLUB_MEMBER_INITIAL_ROWS_PER_PAGE: number = 3 * 4;

type FanClubMemberProfileDisplayContract = Readonly<{
  memberCardAvatarSizePx: number;
}>;

// Invariant: the member avatar remains square and large enough to anchor a
// compact row without changing the card height independently per axis.
const FAN_CLUB_MEMBER_PROFILE_DISPLAY_CONTRACTS = {
  memberCardAvatarSizePx: 7 * 8,
} as const satisfies FanClubMemberProfileDisplayContract;

interface MemberEditForm {
  handle: string;
  bio: string;
  avatarUrl: string;
}

interface MemberCardProps {
  member: FanClubMemberProfileDTO;
  artistId: number;
}

function MemberCard({ member, artistId }: MemberCardProps) {
  return (
    <Card
      component={RouterLink}
      to={`/fans/clubs/${artistId}/members/${member.fcmpPartyId}`}
      sx={{
        textDecoration: 'none',
        transition: 'transform 0.2s, box-shadow 0.2s',
        '&:hover': {
          transform: 'translateY(-2px)',
          boxShadow: (theme) => theme.shadows[4],
        },
      }}
    >
      <CardContent>
        <Stack direction="row" spacing={2} alignItems="center">
          <Avatar
            src={member.fcmpAvatarUrl ?? undefined}
            sx={{
              width: FAN_CLUB_MEMBER_PROFILE_DISPLAY_CONTRACTS.memberCardAvatarSizePx,
              height: FAN_CLUB_MEMBER_PROFILE_DISPLAY_CONTRACTS.memberCardAvatarSizePx,
            }}
          >
            {member.fcmpDisplayName.charAt(0).toUpperCase()}
          </Avatar>
          <Box flexGrow={1} minWidth={0}>
            <Typography variant="subtitle1" fontWeight={600} noWrap>
              {member.fcmpDisplayName}
            </Typography>
            {member.fcmpHandle && (
              <Typography variant="body2" color="text.secondary" noWrap>
                @{member.fcmpHandle}
              </Typography>
            )}
            <Chip size="small" label={`Desde ${new Date(member.fcmpJoinedAt).toLocaleDateString()}`} sx={{ mt: 0.5 }} />
          </Box>
        </Stack>
      </CardContent>
    </Card>
  );
}

interface MemberCardsGridProps {
  members: FanClubMemberProfileDTO[];
  artistId: number;
  loading: boolean;
}

function MemberCardsGrid(props: MemberCardsGridProps) {
  const { members, artistId, loading } = props;

  return (
    <LazyPaginatedList
      items={members}
      loading={loading}
      pagination={{ itemLabel: 'miembros', initialRowsPerPage: FAN_CLUB_MEMBER_INITIAL_ROWS_PER_PAGE }}
      renderItems={(visibleMembers) => (
        <Grid container spacing={2}>
          {visibleMembers.map((member) => (
            <Grid item xs={12} sm={6} md={4} key={member.fcmpPartyId}>
              <MemberCard member={member} artistId={artistId} />
            </Grid>
          ))}
        </Grid>
      )}
    />
  );
}

function MemoryCard({ memory }: { memory: FanClubMemoryDTO }) {
  return (
    <Card>
      <CardContent>
        <Stack spacing={1}>
          <Typography variant="h6">{memory.fcmTitle}</Typography>
          {memory.fcmDescription && (
            <Typography variant="body2" color="text.secondary">
              {memory.fcmDescription}
            </Typography>
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
  );
}

function MemberMemoriesGrid({ memories }: { memories: FanClubMemoryDTO[] }) {
  return (
    <LazyPaginatedList
      items={memories}
      pagination={{ itemLabel: 'recuerdos', initialRowsPerPage: 6 }}
      renderItems={(visibleMemories) => (
        <Grid container spacing={2}>
          {visibleMemories.map(memory => (
            <Grid item xs={12} md={6} key={memory.fcmId}>
              <MemoryCard memory={memory} />
            </Grid>
          ))}
        </Grid>
      )}
    />
  );
}

interface MemberProfileActionsProps {
  isOwnProfile: boolean;
  canMessage: boolean;
  editing: boolean;
  onToggleEdit: () => void;
  onMessage: () => void;
  onOpenInbox: () => void;
}

function MemberProfileActions(props: MemberProfileActionsProps) {
  const { isOwnProfile, canMessage, editing, onToggleEdit, onMessage, onOpenInbox } = props;

  if (isOwnProfile) {
    return (
      <Button
        variant="outlined"
        startIcon={editing ? <CancelIcon /> : <EditIcon />}
        tabIndex={0}
        onClick={(event) => {
          event.currentTarget.focus();
          onToggleEdit();
        }}
      >
        {editing ? 'Cancelar' : 'Editar perfil'}
      </Button>
    );
  }

  if (!canMessage) return null;

  return (
    <Stack direction="row" spacing={1}>
      <Button
        variant="contained"
        tabIndex={0}
        onClick={(event) => {
          event.currentTarget.focus();
          onMessage();
        }}
      >
        Enviar mensaje
      </Button>
      <Button
        variant="outlined"
        startIcon={<MailIcon />}
        tabIndex={0}
        onClick={(event) => {
          event.currentTarget.focus();
          onOpenInbox();
        }}
      >
        Escribir al club
      </Button>
    </Stack>
  );
}

interface MemberProfileEditFormProps {
  form: MemberEditForm;
  saving: boolean;
  onChange: (patch: Partial<MemberEditForm>) => void;
  onSave: () => void;
}

function MemberProfileEditForm(props: MemberProfileEditFormProps) {
  const { form, saving, onChange, onSave } = props;

  return (
    <Stack spacing={2}>
      <TextField
        label="Handle"
        value={form.handle}
        onChange={(event) => onChange({ handle: event.target.value })}
        fullWidth
        placeholder="@tuhandle"
      />
      <TextField
        label="Bio"
        value={form.bio}
        onChange={(event) => onChange({ bio: event.target.value })}
        multiline
        rows={3}
        fullWidth
      />
      <TextField
        label="Avatar URL"
        value={form.avatarUrl}
        onChange={(event) => onChange({ avatarUrl: event.target.value })}
        fullWidth
        placeholder="https://..."
      />
      <Button
        variant="contained"
        startIcon={<SaveIcon />}
        tabIndex={0}
        onClick={(event) => {
          event.currentTarget.focus();
          onSave();
        }}
        disabled={saving}
      >
        Guardar
      </Button>
    </Stack>
  );
}

interface MemberProfileCardProps {
  profile: FanClubMemberProfileDTO;
  isOwnProfile: boolean;
  canMessage: boolean;
  editing: boolean;
  editForm: MemberEditForm;
  saving: boolean;
  onEditFormChange: (patch: Partial<MemberEditForm>) => void;
  onToggleEdit: () => void;
  onSave: () => void;
  onMessage: () => void;
  onOpenInbox: () => void;
}

function MemberProfileCard(props: MemberProfileCardProps) {
  const {
    profile,
    isOwnProfile,
    canMessage,
    editing,
    editForm,
    saving,
    onEditFormChange,
    onToggleEdit,
    onSave,
    onMessage,
    onOpenInbox,
  } = props;

  return (
    <Card>
      <CardContent>
        <Stack spacing={2}>
          <Stack direction="row" spacing={2} alignItems="center">
            <Avatar src={profile.fcmpAvatarUrl ?? undefined} sx={{ width: 80, height: 80 }} />
            <Box flexGrow={1}>
              <Typography variant="h5" fontWeight={600}>{profile.fcmpDisplayName}</Typography>
              {profile.fcmpHandle && (
                <Typography variant="subtitle1" color="text.secondary">@{profile.fcmpHandle}</Typography>
              )}
              <Chip size="small" label={`Miembro desde ${new Date(profile.fcmpJoinedAt).toLocaleDateString()}`} />
            </Box>
            <MemberProfileActions
              isOwnProfile={isOwnProfile}
              canMessage={canMessage}
              editing={editing}
              onToggleEdit={onToggleEdit}
              onMessage={onMessage}
              onOpenInbox={onOpenInbox}
            />
          </Stack>

          {editing ? (
            <MemberProfileEditForm
              form={editForm}
              saving={saving}
              onChange={onEditFormChange}
              onSave={onSave}
            />
          ) : (
            profile.fcmpBio && <Typography variant="body1">{profile.fcmpBio}</Typography>
          )}
        </Stack>
      </CardContent>
    </Card>
  );
}

interface InboxMessageDialogProps {
  open: boolean;
  subject: string;
  body: string;
  sending: boolean;
  onClose: () => void;
  onSubjectChange: (value: string) => void;
  onBodyChange: (value: string) => void;
  onSend: () => void;
}

function InboxMessageDialog(props: InboxMessageDialogProps) {
  const { open, subject, body, sending, onClose, onSubjectChange, onBodyChange, onSend } = props;

  return (
    <Dialog open={open} onClose={onClose} fullWidth maxWidth="sm">
      <DialogTitle>Escribir al club</DialogTitle>
      <DialogContent>
        <Stack spacing={2} sx={{ mt: 1 }}>
          <TextField label="Asunto (opcional)" value={subject} onChange={(event) => onSubjectChange(event.target.value)} fullWidth />
          <TextField label="Mensaje" value={body} onChange={(event) => onBodyChange(event.target.value)} multiline rows={4} fullWidth required />
        </Stack>
      </DialogContent>
      <DialogActions>
        <Button
          tabIndex={0}
          onClick={(event) => {
            event.currentTarget.focus();
            onClose();
          }}
        >
          Cancelar
        </Button>
        <Button
          variant="contained"
          tabIndex={0}
          onClick={(event) => {
            event.currentTarget.focus();
            onSend();
          }}
          disabled={!body.trim() || sending}
        >
          Enviar
        </Button>
      </DialogActions>
    </Dialog>
  );
}

export default function FanClubMemberProfilePage() {
  const { artistId, partyId } = useParams<{ artistId: string; partyId: string }>();
  const artistIdNum = parseInt(artistId ?? '0', 10);
  const partyIdNum = parseInt(partyId ?? '0', 10);
  const { session } = useSession();
  const isAuthenticated = Boolean(session);
  const navigate = useNavigate();
  const qc = useQueryClient();
  const [editing, setEditing] = useState(false);
  const [editForm, setEditForm] = useState<MemberEditForm>({ handle: '', bio: '', avatarUrl: '' });
  const [inboxOpen, setInboxOpen] = useState(false);
  const [inboxSubject, setInboxSubject] = useState('');
  const [inboxBody, setInboxBody] = useState('');

  const sendInboxMessage = useMutation({
    mutationFn: () => Fans.sendClubInboxMessage(artistIdNum, {
      fcisReqSubject: inboxSubject.trim() ? inboxSubject : null,
      fcisReqBody: inboxBody,
    }),
    onSuccess: () => {
      setInboxOpen(false);
      setInboxSubject('');
      setInboxBody('');
    },
  });

  const clubQuery = useQuery({
    queryKey: ['fan-club', artistIdNum, isAuthenticated ? 'auth' : 'public'],
    queryFn: () => isAuthenticated ? Fans.getMyClub(artistIdNum) : Fans.getClub(artistIdNum),
    enabled: artistIdNum > 0,
  });

  const profilesQuery = useQuery({
    queryKey: ['fan-club-member-profiles', artistIdNum],
    queryFn: () => Fans.listClubMemberProfiles(artistIdNum),
    enabled: artistIdNum > 0 && isAuthenticated,
  });

  const myProfileQuery = useQuery({
    queryKey: ['fan-club-my-member-profile', artistIdNum],
    queryFn: () => Fans.getMyClubMemberProfile(artistIdNum),
    enabled: artistIdNum > 0 && isAuthenticated,
  });

  const memoriesQuery = useQuery({
    queryKey: ['fan-club-memories', artistIdNum],
    queryFn: () => Fans.listClubMemories(artistIdNum),
    enabled: artistIdNum > 0 && isAuthenticated,
  });

  const isMe = session?.partyId === partyIdNum;
  const profile = profilesQuery.data?.find(p => p.fcmpPartyId === partyIdNum);
  const myProfile = myProfileQuery.data;

  const updateProfile = useMutation({
    mutationFn: () => Fans.updateMyClubMemberProfile(artistIdNum, {
      fcmpuHandle: editForm.handle.trim() ? editForm.handle : null,
      fcmpuBio: editForm.bio.trim() ? editForm.bio : null,
      fcmpuAvatarUrl: editForm.avatarUrl.trim() ? editForm.avatarUrl : null,
    }),
    onSuccess: () => {
      void qc.invalidateQueries({ queryKey: ['fan-club-member-profiles', artistIdNum] });
      void qc.invalidateQueries({ queryKey: ['fan-club-my-member-profile', artistIdNum] });
      setEditing(false);
    },
  });

  const memberMemories = memoriesQuery.data?.filter(m => m.fcmMemberProfileId === profile?.fcmpId && !m.fcmIsHidden && !m.fcmIsDeleted) ?? [];

  const toggleEditing = () => {
    if (editing) {
      setEditing(false);
      return;
    }

    setEditForm({
      handle: myProfile?.fcmpHandle ?? '',
      bio: myProfile?.fcmpBio ?? '',
      avatarUrl: myProfile?.fcmpAvatarUrl ?? '',
    });
    setEditing(true);
  };

  const openDirectMessage = () => {
    if (!profile) return;

    ChatAPI.getOrCreateDmThread(profile.fcmpPartyId)
      .then(() => {
        navigate('/chat');
      })
      .catch(() => {
        // ignore errors
      });
  };

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

  // If no partyId is provided, show the members list
  if (!partyId) {
    return (
      <PageShell
        title="Miembros del club"
        subtitle={clubQuery.data ? `${profilesQuery.data?.length ?? 0} miembros` : undefined}
        loading={clubQuery.isLoading || profilesQuery.isLoading}
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

          {!profilesQuery.data || profilesQuery.data.length === 0 ? (
            <EmptyState
              icon={<PhotoLibraryIcon fontSize="large" />}
              title="Sin miembros"
              description="Aún no hay miembros en este club de fans."
            />
          ) : (
            <MemberCardsGrid members={profilesQuery.data} artistId={artistIdNum} loading={profilesQuery.isFetching} />
          )}
        </Stack>
      </PageShell>
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
      subtitle={profile.fcmpHandle ?? `@miembro-${profile.fcmpPartyId}`}
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

        <MemberProfileCard
          profile={profile}
          isOwnProfile={isMe}
          canMessage={Boolean(!isMe && session)}
          editing={editing}
          editForm={editForm}
          saving={updateProfile.isPending}
          onEditFormChange={(patch) => setEditForm((form) => ({ ...form, ...patch }))}
          onToggleEdit={toggleEditing}
          onSave={() => updateProfile.mutate()}
          onMessage={openDirectMessage}
          onOpenInbox={() => setInboxOpen(true)}
        />

        <Typography variant="h6" fontWeight={600}>Recuerdos</Typography>
        {memberMemories.length === 0 ? (
          <EmptyState
            icon={<PhotoLibraryIcon fontSize="large" />}
            title="Sin recuerdos"
            description="Este miembro aún no ha compartido recuerdos."
          />
        ) : (
          <MemberMemoriesGrid memories={memberMemories} />
        )}

        <InboxMessageDialog
          open={inboxOpen}
          subject={inboxSubject}
          body={inboxBody}
          sending={sendInboxMessage.isPending}
          onClose={() => setInboxOpen(false)}
          onSubjectChange={setInboxSubject}
          onBodyChange={setInboxBody}
          onSend={() => sendInboxMessage.mutate()}
        />
      </Stack>
    </PageShell>
  );
}
