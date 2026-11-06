import { useMemo } from 'react';
import { useOwnedProfileDraft } from './useOwnedProfileDraft';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { Fans } from '../../api/fans';
import type { FanProfileUpdate } from '../../api/types';

const emptyFanProfileDraft: FanProfileUpdate = {
  fpuDisplayName: '',
  fpuBio: '',
  fpuCity: '',
  fpuFavoriteGenreIds: [],
  fpuAvatarUrl: '',
};

export function useFanProfile({
  enabled,
  viewerId,
  onProfileSaved,
}: {
  enabled: boolean;
  viewerId: number | null;
  onProfileSaved?: (profile: FanProfileUpdate) => void;
}) {
  const qc = useQueryClient();
  const profileQuery = useQuery({
    queryKey: ['fan-profile', viewerId],
    queryFn: Fans.getProfile,
    enabled,
  });
  const baseline = useMemo(() => {
    const profile = profileQuery.data;
    if (!enabled || !profileQuery.isSuccess || profile?.fpArtistId !== viewerId) return undefined;
    return {
      fpuDisplayName: profile.fpDisplayName ?? '',
      fpuBio: profile.fpBio ?? '',
      fpuCity: profile.fpCity ?? '',
      fpuFavoriteGenreIds: profile.fpFavoriteGenreIds ?? [],
      fpuAvatarUrl: profile.fpAvatarUrl ?? '',
    };
  }, [enabled, profileQuery.data, profileQuery.isSuccess, viewerId]);
  const {
    draft: profileDraft, setDraft: setProfileDraft, ready: profileReady,
    acknowledgeSave, isCurrentOwner,
  } = useOwnedProfileDraft<FanProfileUpdate>({
    ownerId: enabled ? viewerId : null, baseline, empty: emptyFanProfileDraft,
  });

  const updateProfileMutation = useMutation({
    mutationFn: (request: {
      profile: FanProfileUpdate;
      ownerId: number;
      isCurrentOwner: () => boolean;
      acknowledgeSave: (profile: FanProfileUpdate) => void;
    }) => {
      if (!request.isCurrentOwner()) throw new Error('Vuelve a cargar tu perfil antes de guardar.');
      return Fans.updateProfile(request.profile);
    },
    onSuccess: (_profile, request) => {
      void qc.invalidateQueries({ queryKey: ['fan-profile', request.ownerId] });
      if (!request.isCurrentOwner()) return;
      request.acknowledgeSave(request.profile);
      onProfileSaved?.(request.profile);
    },
  });

  const saveProfile = () => {
    if (!profileReady || !isCurrentOwner() || !viewerId || updateProfileMutation.isPending) return;
    updateProfileMutation.mutate({ profile: profileDraft, ownerId: viewerId, isCurrentOwner, acknowledgeSave });
  };

  return {
    profileDraft,
    profileQuery,
    profileReady,
    saveProfile,
    setProfileDraft,
    updateProfileMutation,
  };
}
