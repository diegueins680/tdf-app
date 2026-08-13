import type { KeyboardEvent, MouseEvent } from 'react';
import { CircularProgress, IconButton, Stack, Typography, Tooltip } from '@mui/material';
import { useTranslation } from 'react-i18next';
import type { ReactionSummaryDTO } from '../api/types';

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

function isActivationKey(key: string): boolean {
  return key === 'Enter' || key === ' ';
}

function focusSoon(getTarget: () => HTMLElement | null): void {
  globalThis.setTimeout(() => getTarget()?.focus(), 0);
}

interface ReactionBarProps {
  reactions: ReactionSummaryDTO;
  onReact: (reactionTypeId: string) => void;
  disabled?: boolean;
  loading?: boolean;
}

export default function ReactionBar({ reactions, onReact, disabled, loading = false }: ReactionBarProps) {
  const { i18n, t } = useTranslation();
  const english = (i18n.resolvedLanguage ?? i18n.language).toLowerCase().startsWith('en');
  const reactionOptions = reactions.rsItems;
  const isEmpty = reactionOptions.length === 0;
  const isDisabled = loading ? true : (disabled ?? false);

  const focusAfterReact = (target: HTMLButtonElement, reactionTypeId: string) => {
    if (isDisabled) return;
    onReact(reactionTypeId);
    focusSoon(() => target);
  };

  const focusAfterReactKeyDown = (event: KeyboardEvent<HTMLButtonElement>, reactionTypeId: string) => {
    if (!isActivationKey(event.key)) return;
    event.preventDefault();
    focusAfterReact(event.currentTarget, reactionTypeId);
  };
  const focus = {
    afterReact: focusAfterReact,
    afterReactKeyDown: focusAfterReactKeyDown,
  };

  if (isEmpty) {
    return (
      <Typography variant="caption" color="text.secondary" role="status">
        {t('fanClub.reactions.empty')}
      </Typography>
    );
  }

  return (
    <Stack direction="row" spacing={0.5} alignItems="center" aria-busy={loading ? true : undefined}>
      {reactionOptions.map((option) => {
        const count = option.rsiCount;
        const label = english ? option.rsiNameEn : option.rsiNameEs;
        const isActive = reactions.rsMyReactionTypeId === option.rsiReactionTypeId;
        return (
          <Tooltip key={option.rsiReactionTypeId} title={label}>
            <IconButton
              size="small"
              onClick={(event: MouseEvent<HTMLButtonElement>) => focus.afterReact(event.currentTarget, option.rsiReactionTypeId)}
              onKeyDown={(event) => focus.afterReactKeyDown(event, option.rsiReactionTypeId)}
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
                {option.rsiDisplaySymbol}
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
          aria-label={t('fanClub.reactions.loading')}
        />
      )}
    </Stack>
  );
}
