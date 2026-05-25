import type { KeyboardEvent, MouseEvent } from 'react';
import { CircularProgress, IconButton, Stack, Typography, Tooltip } from '@mui/material';
import type { ReactionSummaryDTO } from '../api/types';

const REACTIONS = [
  { key: 'fire', emoji: '🔥', label: 'Fuego' },
  { key: 'heart', emoji: '❤️', label: 'Me encanta' },
  { key: 'clap', emoji: '👏', label: 'Aplauso' },
  { key: 'mic_drop', emoji: '🎤', label: 'Mic drop' },
  { key: 'skull', emoji: '💀', label: 'Me muero' },
] as const;

type ReactionKey = typeof REACTIONS[number]['key'];
type ReactionDefinition = typeof REACTIONS[number];

type ReactionBarContract = Readonly<{
  activeCountFontWeight: number;
  inactiveCountFontWeight: number;
  loadingSpinnerSizePx: number;
}>;

// Invariant: active reaction counts are visually heavier than inactive counts,
// and both values remain valid CSS numeric font weights.
export const REACTION_BAR_CONTRACTS = {
  activeCountFontWeight: 7 * 100,
  inactiveCountFontWeight: 4 * 100,
  loadingSpinnerSizePx: 4 * 4,
} as const satisfies ReactionBarContract;

const REACTION_BAR_COPY = {
  empty: 'Sin reacciones disponibles',
  loading: 'Guardando reacción',
} as const;

function isActivationKey(key: string): boolean {
  return key === 'Enter' || key === ' ';
}

function focusSoon(getTarget: () => HTMLElement | null): void {
  globalThis.setTimeout(() => getTarget()?.focus(), 0);
}

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
  loading?: boolean;
}

export default function ReactionBar({ reactions, onReact, disabled, loading = false }: ReactionBarProps) {
  const reactionOptions: readonly ReactionDefinition[] = REACTIONS;
  const isEmpty = reactionOptions.length === 0;
  const isDisabled = Boolean(disabled || loading);

  const focusAfterReact = (target: HTMLButtonElement, reaction: ReactionKey) => {
    if (isDisabled) return;
    onReact(reaction);
    focusSoon(() => target);
  };

  const focusAfterReactKeyDown = (event: KeyboardEvent<HTMLButtonElement>, reaction: ReactionKey) => {
    if (!isActivationKey(event.key)) return;
    event.preventDefault();
    focusAfterReact(event.currentTarget, reaction);
  };
  const focus = {
    afterReact: focusAfterReact,
    afterReactKeyDown: focusAfterReactKeyDown,
  };

  if (isEmpty) {
    return (
      <Typography variant="caption" color="text.secondary" role="status">
        {REACTION_BAR_COPY.empty}
      </Typography>
    );
  }

  return (
    <Stack direction="row" spacing={0.5} alignItems="center" aria-busy={loading ? true : undefined}>
      {reactionOptions.map(({ key, emoji, label }) => {
        const count = getCount(reactions, key);
        const isActive = reactions.rsMyReaction === key;
        return (
          <Tooltip key={key} title={label}>
            <IconButton
              size="small"
              onClick={(event: MouseEvent<HTMLButtonElement>) => focus.afterReact(event.currentTarget, key)}
              onKeyDown={(event) => focus.afterReactKeyDown(event, key)}
              disabled={isDisabled}
              aria-label={count > 0 ? `${label} (${count})` : label}
              aria-pressed={isActive}
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
                  sx={{
                    ml: 0.5,
                    fontWeight: isActive
                      ? REACTION_BAR_CONTRACTS.activeCountFontWeight
                      : REACTION_BAR_CONTRACTS.inactiveCountFontWeight,
                    color: isActive ? 'primary.main' : 'text.secondary',
                  }}
                >
                  {count}
                </Typography>
              )}
            </IconButton>
          </Tooltip>
        );
      })}
      {loading && (
        <CircularProgress
          size={REACTION_BAR_CONTRACTS.loadingSpinnerSizePx}
          aria-label={REACTION_BAR_COPY.loading}
        />
      )}
    </Stack>
  );
}
