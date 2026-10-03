import { useMutation, useQueryClient } from '@tanstack/react-query';
import { Alert, Stack } from '@mui/material';
import ReactionBar from '../../components/ReactionBar';
import { Fans } from '../../api/fans';
import type { FanClubFeedItemDTO } from '../../api/types';

/** Mounted only when the server explicitly confirms first activation has not occurred. */
export function LegacyClubReactions({ artistId, item }: { artistId: number; item: FanClubFeedItemDTO }) {
  const client = useQueryClient();
  const reaction = useMutation({
    mutationFn: (reactionTypeId: string) => item.fcfKind === 'post'
      ? Fans.reactToPost(artistId, item.fcfId, { crrReactionTypeId: reactionTypeId })
      : Fans.reactToMemory(artistId, item.fcfId, { crrReactionTypeId: reactionTypeId }),
    onSettled: async () => {
      await Promise.all([
        client.invalidateQueries({ queryKey: ['fan-club-feed', artistId] }),
        client.invalidateQueries({ queryKey: ['interactions'] }),
      ]);
    },
  });
  return <Stack spacing={1}>
    <ReactionBar reactions={item.fcfReactions} onReact={(id) => reaction.mutate(id)} loading={reaction.isPending} />
    {reaction.isError && <Alert severity="error">No se pudo guardar tu reacción. Vuelve a intentarlo.</Alert>}
  </Stack>;
}
