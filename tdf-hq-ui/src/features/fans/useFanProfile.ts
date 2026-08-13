import { useEffect, useState } from 'react';
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
  const [profileDraft, setProfileDraft] = useState<FanProfileUpdate>(emptyFanProfileDraft);

  useEffect(() => {
    if (profileQuery.data) {
      setProfileDraft({
        fpuDisplayName: profileQuery.data.fpDisplayName ?? '',
        fpuBio: profileQuery.data.fpBio ?? '',
        fpuCity: profileQuery.data.fpCity ?? '',
        fpuFavoriteGenreIds: profileQuery.data.fpFavoriteGenreIds ?? [],
        fpuAvatarUrl: profileQuery.data.fpAvatarUrl ?? '',
      });
    }
  }, [profileQuery.data]);

  const updateProfileMutation = useMutation({
    mutationFn: Fans.updateProfile,
    onSuccess: (_profile, variables) => {
      void qc.invalidateQueries({ queryKey: ['fan-profile', viewerId] });
      onProfileSaved?.(variables);
    },
  });

  const saveProfile = () => {
    updateProfileMutation.mutate(profileDraft);
  };

  return {
    profileDraft,
    profileQuery,
    saveProfile,
    setProfileDraft,
    updateProfileMutation,
  };
}
