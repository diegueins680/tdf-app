import { IconButton, Stack, Typography, Tooltip } from '@mui/material';
import type { ReactionSummaryDTO } from '../api/types';

const REACTIONS = [
  { key: 'fire', emoji: '🔥', label: 'Fuego' },
  { key: 'heart', emoji: '❤️', label: 'Me encanta' },
  { key: 'clap', emoji: '👏', label: 'Aplauso' },
  { key: 'mic_drop', emoji: '🎤', label: 'Mic drop' },
  { key: 'skull', emoji: '💀', label: 'Me muero' },
] as const;

type ReactionKey = typeof REACTIONS[number]['key'];

function getCount(reactions: ReactionSummaryDTO, key: ReactionKey): number {
  switch (key) {
    case 'fire': return reactions.rsFire;
    case 'heart': return reactions.rsHeart;
    case 'clap': return reactions.rsClap;
    case 'mic_drop': return reactions.rsMicDrop;
    case 'skull': return reactions.rsSkull;
  }
}

interface ReactionBarProps {
  reactions: ReactionSummaryDTO;
  onReact: (reaction: string) => void;
  disabled?: boolean;
}

export default function ReactionBar({ reactions, onReact, disabled }: ReactionBarProps) {
  return (
    <Stack direction="row" spacing={0.5} alignItems="center">
      {REACTIONS.map(({ key, emoji, label }) => {
        const count = getCount(reactions, key);
        const isActive = reactions.rsMyReaction === key;
        return (
          <Tooltip key={key} title={label}>
            <IconButton
              size="small"
              onClick={() => onReact(key)}
              disabled={disabled}
              sx={{
                borderRadius: '16px',
                px: 1,
                py: 0.25,
                bgcolor: isActive ? 'action.selected' : 'transparent',
                border: isActive ? 1 : 0,
                borderColor: 'primary.main',
                '&:hover': { bgcolor: 'action.hover' },
              }}
            >
              <Typography variant="caption" sx={{ fontSize: '1rem', lineHeight: 1 }}>
                {emoji}
              </Typography>
              {count > 0 && (
                <Typography
                  variant="caption"
                  sx={{ ml: 0.5, fontWeight: isActive ? 700 : 400, color: isActive ? 'primary.main' : 'text.secondary' }}
                >
                  {count}
                </Typography>
              )}
            </IconButton>
          </Tooltip>
        );
      })}
    </Stack>
  );
}
