import { useEffect, useId, useRef, useState } from 'react';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { z } from 'zod';
import { completeOnboardingProgress, loadOnboardingProgress, type OnboardingProgressDTO } from '../../api/session';
import { getActiveSession, type SessionUser } from '../../session/SessionContext';

const timestamp = z.string().datetime({ offset: true }).nullable();
// Executable wire contract, not a second source of onboarding completion authority.
const progressSchema = z.object({
  eligible: z.boolean(),
  signupCompletedAt: timestamp,
  onboardingIntent: z.enum(['events', 'follow_artists', 'artist_profile', 'internships', 'learning', 'professional_tools']).nullable(),
  completedAt: timestamp,
  firstValue: z.enum(['artist_followed', 'access_requested', 'event_saved', 'moment_reaction']).nullable(),
  firstValueCompletedAt: timestamp,
  updatedAt: timestamp,
}).refine((progress) => !progress.eligible || (progress.completedAt === null && progress.signupCompletedAt !== null)) satisfies z.ZodType<OnboardingProgressDTO>;
const receiptSchema = z.object({ progress: progressSchema, newlyCompleted: z.boolean() })
  .refine((receipt) => !receipt.progress.eligible)
  .refine((receipt) => !receipt.newlyCompleted || receipt.progress.completedAt !== null);

interface ExitCommand {
  generation: number;
  isCurrent: () => boolean;
  queryKey: readonly unknown[];
  complete: () => ReturnType<typeof completeOnboardingProgress>;
}

export function useFanHubOnboarding(session: SessionUser | null, loading: boolean, manager: boolean) {
  const queryClient = useQueryClient();
  const instance = useId();
  const mounted = useRef(true);
  const flights = useRef(new Set<number>());
  const mode = loading ? 'waiting' : !session ? 'guest'
    : !Number.isSafeInteger(session.partyId) || (session.partyId ?? 0) <= 0 ? 'waiting'
      : manager ? 'manager' : 'account';
  const context = useRef({ session, mode, generation: 0 });
  const generation = context.current.generation
    + Number(context.current.session !== session || context.current.mode !== mode);
  context.current = { session, mode, generation };
  const [dismissedGeneration, setDismissedGeneration] = useState<number | null>(null);
  useEffect(() => {
    mounted.current = true;
    return () => { mounted.current = false; };
  }, []);
  const isCurrent = () => mounted.current && context.current.generation === generation
    && getActiveSession() === session;
  // Session objects/tokens never enter cache keys. No previous-generation placeholder data.
  const queryKey = ['onboarding-progress', session?.partyId, instance, generation] as const;
  const progress = useQuery({
    queryKey,
    queryFn: async () => {
      if (!isCurrent()) throw new Error('Onboarding context changed');
      const result = await loadOnboardingProgress(session?.apiToken ?? undefined);
      if (!isCurrent()) throw new Error('Onboarding context changed');
      return progressSchema.parse(result);
    },
    enabled: mode === 'account',
    retry: false,
    gcTime: 0,
  });
  const exit = useMutation({
    mutationFn: async (command: ExitCommand) => {
      if (!command.isCurrent()) throw new Error('Onboarding context changed');
      return receiptSchema.parse(await command.complete());
    },
    onSuccess: (receipt, command) => {
      if (!command.isCurrent()) return;
      // A GET started before this receipt must not reopen already-acknowledged guidance.
      setDismissedGeneration(command.generation);
      queryClient.setQueryData(command.queryKey, receipt.progress);
    },
    onSettled: (_data, _error, command) => { flights.current.delete(command.generation); },
  });
  const currentExit = exit.variables?.generation === generation;
  const saving = currentExit && exit.isPending;
  const saveError = currentExit && exit.isError;
  const eligible = dismissedGeneration !== generation && progress.isSuccess && !progress.isFetching && progress.data.eligible;
  const visible = mode === 'account' ? eligible
    : (mode === 'guest' || mode === 'manager') && dismissedGeneration !== generation;
  const dismiss = () => {
    if (!isCurrent()) return;
    if (mode === 'guest' || mode === 'manager') {
      setDismissedGeneration(generation);
      return;
    }
    if (mode !== 'account' || !eligible || flights.current.has(generation)) return;
    flights.current.add(generation);
    const token = session?.apiToken ?? undefined;
    exit.mutate({ generation, isCurrent, queryKey, complete: () => completeOnboardingProgress(undefined, token) });
  };
  const retryLoad = () => {
    if (mode === 'account' && isCurrent() && !progress.isFetching) void progress.refetch();
  };
  return {
    visible,
    saving,
    saveError: mode === 'account' && saveError && eligible,
    loadError: mode === 'account' && dismissedGeneration !== generation && progress.isError,
    loading: mode === 'account' && dismissedGeneration !== generation && progress.isFetching,
    dismiss,
    retryLoad,
  };
}
