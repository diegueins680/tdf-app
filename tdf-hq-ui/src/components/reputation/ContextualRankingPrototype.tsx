import { useEffect, useMemo, useReducer, useRef, type KeyboardEvent } from 'react';
import { useTranslation } from 'react-i18next';
import DragIndicatorIcon from '@mui/icons-material/DragIndicator';
import VerifiedIcon from '@mui/icons-material/Verified';
import { Avatar, Box, Button, Chip, IconButton, Stack, Typography } from '@mui/material';

export interface RankingPerson {
  id: string; name: string; professionalName?: string; role: string; city?: string;
  interaction: string; verified: boolean; avatarUrl?: string;
}
interface Props { category: string; people: RankingPerson[]; onSave?: (orderedIds: string[]) => Promise<void> | void }
interface RankingState {
  order: RankingPerson[]; previous: RankingPerson[] | null; draggedId: string | null;
  excluded: Set<string>; status: string; isSaving: boolean; focusTarget: string | null;
}
type Action =
  | { type: 'commit'; order: RankingPerson[]; message: string; focusTarget: string }
  | { type: 'drag'; id: string | null }
  | { type: 'exclude'; person: RankingPerson; message: string }
  | { type: 'saving'; isSaving: boolean }
  | { type: 'status'; status: string; focusTarget: string }
  | { type: 'undo'; message: string } | { type: 'clearFocus' };

const initials = (name: string) => name.split(/\s+/).filter(Boolean).slice(0, 2).map((part) => part[0]).join('').toUpperCase();

function reducer(state: RankingState, action: Action): RankingState {
  switch (action.type) {
    case 'commit': return { ...state, previous: state.order, order: action.order, status: action.message, focusTarget: action.focusTarget };
    case 'drag': return { ...state, draggedId: action.id };
    case 'exclude': {
      const excluded = new Set(state.excluded);
      if (excluded.has(action.person.id)) excluded.delete(action.person.id); else excluded.add(action.person.id);
      return { ...state, excluded, status: action.message, focusTarget: `exclude-${action.person.id}` };
    }
    case 'saving': return { ...state, isSaving: action.isSaving };
    case 'status': return { ...state, status: action.status, focusTarget: action.focusTarget };
    case 'undo':
      return state.previous
        ? { ...state, order: state.previous, previous: null, status: action.message, focusTarget: 'undo' }
        : state;
    case 'clearFocus': return { ...state, focusTarget: null };
  }
}

function activateWithKeyboard(event: KeyboardEvent<HTMLButtonElement>) {
  if (event.key === 'Enter' || event.key === ' ') {
    event.preventDefault();
    event.currentTarget.click();
  }
}

interface RankingRowProps {
  person: RankingPerson; position: number; isExcluded: boolean; isFirst: boolean; isLast: boolean;
  actionDisabled: boolean;
  onMove: (id: string, delta: number) => void; onToggleExcluded: (person: RankingPerson) => void;
  registerButton: (target: string, element: HTMLButtonElement | null) => void;
}

function RankingRow({
  person, position, isExcluded, isFirst, isLast, actionDisabled, onMove, onToggleExcluded, registerButton,
}: RankingRowProps) {
  const { t } = useTranslation();
  return (
    <Box sx={{
      opacity: isExcluded ? 0.6 : 1, border: 1, borderColor: 'divider', borderRadius: 2, p: 1.25, bgcolor: 'background.paper',
    }}>
      <Stack direction="row" alignItems="center" spacing={1}>
        <Typography aria-label={isExcluded
          ? t('contextualRanking.excludedPosition', { name: person.name })
          : t('contextualRanking.position', { position })} fontWeight={800} sx={{ minWidth: 28 }}>
          {isExcluded ? '—' : position}
        </Typography>
        <DragIndicatorIcon aria-hidden="true" color="action" />
        <Avatar src={person.avatarUrl} alt="">{initials(person.name)}</Avatar>
        <Box sx={{ flex: 1, minWidth: 0 }}>
          <Typography fontWeight={700}>{person.name}{person.professionalName ? ` · ${person.professionalName}` : ''}</Typography>
          <Typography variant="body2" color="text.secondary">
            {person.role}{person.city ? ` · ${person.city}` : ''} · {person.interaction}
          </Typography>
        </Box>
        {person.verified && <Chip icon={<VerifiedIcon />} label={t('contextualRanking.verified')} size="small" color="success" variant="outlined" />}
      </Stack>
      <Stack direction="row" spacing={0.75} sx={{ mt: 1, ml: { sm: 5 } }}>
        <IconButton
          ref={(element) => registerButton(`up-${person.id}`, element)}
          aria-label={t('contextualRanking.moveUp', { name: person.name })}
          onClick={() => onMove(person.id, -1)}
          onKeyDown={activateWithKeyboard}
          disabled={isFirst || isExcluded || actionDisabled}
        >
          ↑
        </IconButton>
        <IconButton
          ref={(element) => registerButton(`down-${person.id}`, element)}
          aria-label={t('contextualRanking.moveDown', { name: person.name })}
          onClick={() => onMove(person.id, 1)}
          onKeyDown={activateWithKeyboard}
          disabled={isLast || isExcluded || actionDisabled}
        >
          ↓
        </IconButton>
        <Button
          ref={(element) => registerButton(`exclude-${person.id}`, element)}
          size="small"
          onClick={() => onToggleExcluded(person)}
          onKeyDown={activateWithKeyboard}
          disabled={actionDisabled}
        >
          {isExcluded ? t('contextualRanking.include') : t('contextualRanking.insufficientInformation')}
        </Button>
      </Stack>
    </Box>
  );
}

/** Native drag is progressive enhancement; position controls work by keyboard and touch. */
export default function ContextualRankingPrototype({ category, people, onSave }: Props) {
  const { t } = useTranslation();
  const [state, dispatch] = useReducer(reducer, people, (initialPeople): RankingState => ({
    order: [...initialPeople].sort(() => Math.random() - 0.5), previous: null, draggedId: null,
    excluded: new Set(), status: '', isSaving: false, focusTarget: null,
  }));
  const buttonRefs = useRef(new Map<string, HTMLButtonElement>());
  const visible = useMemo(() => state.order.filter((person) => !state.excluded.has(person.id)), [state.excluded, state.order]);

  useEffect(() => {
    if (state.focusTarget) {
      buttonRefs.current.get(state.focusTarget)?.focus();
      dispatch({ type: 'clearFocus' });
    }
  }, [state.focusTarget]);

  const registerButton = (target: string, element: HTMLButtonElement | null) => {
    if (element) buttonRefs.current.set(target, element); else buttonRefs.current.delete(target);
  };
  const commit = (order: RankingPerson[], message: string, focusTarget: string) => {
    dispatch({ type: 'commit', order, message, focusTarget });
  };
  const move = (id: string, delta: number) => {
    const index = state.order.findIndex((person) => person.id === id);
    const nextIndex = index + delta;
    if (index < 0 || nextIndex < 0 || nextIndex >= state.order.length) return;
    const order = [...state.order];
    const person = order[index];
    const adjacentPerson = order[nextIndex];
    if (!person || !adjacentPerson) return;
    [order[index], order[nextIndex]] = [adjacentPerson, person];
    commit(order, t('contextualRanking.moved', { name: person.name, position: nextIndex + 1 }), `${delta < 0 ? 'up' : 'down'}-${id}`);
  };
  const toggleExcluded = (person: RankingPerson) => {
    const isExcluded = state.excluded.has(person.id);
    const message = isExcluded
      ? t('contextualRanking.included', { name: person.name })
      : t('contextualRanking.excluded', { name: person.name });
    dispatch({ type: 'exclude', person, message });
  };
  const save = async () => {
    if (state.isSaving) return;
    dispatch({ type: 'saving', isSaving: true });
    try {
      await onSave?.(visible.map((person) => person.id));
      dispatch({ type: 'status', status: t('contextualRanking.saved'), focusTarget: 'save' });
    } catch {
      dispatch({ type: 'status', status: t('contextualRanking.saveError'), focusTarget: 'save' });
    } finally { dispatch({ type: 'saving', isSaving: false }); }
  };
  const dropAt = (index: number) => {
    const from = state.order.findIndex((person) => person.id === state.draggedId);
    if (from < 0 || from === index || !state.draggedId) return;
    const order = [...state.order];
    const [person] = order.splice(from, 1);
    if (!person) return;
    order.splice(index, 0, person);
    commit(order, t('contextualRanking.moved', { name: person.name, position: index + 1 }), `down-${person.id}`);
    dispatch({ type: 'drag', id: null });
  };

  return (
    <Box component="section" aria-labelledby="contextual-ranking-title" sx={{ maxWidth: 760, mx: 'auto', p: { xs: 2, sm: 3 } }}>
      <Typography id="contextual-ranking-title" variant="h5" fontWeight={800}>{t('contextualRanking.title', { category })}</Typography>
      <Typography color="text.secondary" sx={{ mt: 0.5 }}>
        {t('contextualRanking.instructions')}
      </Typography>
      <Typography color="text.secondary" variant="body2" sx={{ mt: 1 }}>
        {t('contextualRanking.comparabilityHint')}
      </Typography>
      <Box role="status" aria-live="polite" sx={{
        position: 'absolute', width: 1, height: 1, overflow: 'hidden', clip: 'rect(0 0 0 0)',
      }}>
        {state.status}
      </Box>
      {state.order.length === 0 ? (
        <Typography color="text.secondary" sx={{ mt: 3 }}>
          {t('contextualRanking.empty')}
        </Typography>
      ) : (
        <Stack component="ol" aria-label={t('contextualRanking.listLabel', { category })} spacing={1.25} sx={{ listStyle: 'none', p: 0, mt: 2 }}>
          {state.order.map((person, index) => {
            const isExcluded = state.excluded.has(person.id);
            return (
              <Box
                component="li"
                key={person.id}
                draggable={!isExcluded}
                onDragStart={() => dispatch({ type: 'drag', id: person.id })}
                onDragOver={(event) => event.preventDefault()}
                onDrop={() => dropAt(index)}
              >
                <RankingRow
                  person={person}
                  position={visible.findIndex((item) => item.id === person.id) + 1}
                  isExcluded={isExcluded}
                  isFirst={index === 0}
                  isLast={index === state.order.length - 1}
                  actionDisabled={state.isSaving}
                  onMove={move}
                  onToggleExcluded={toggleExcluded}
                  registerButton={registerButton}
                />
              </Box>
            );
          })}
        </Stack>
      )}
      <Stack direction="row" spacing={1} sx={{ mt: 2 }}>
        <Button
          ref={(element) => registerButton('save', element)}
          variant="contained"
          onClick={() => { void save(); }}
          onKeyDown={activateWithKeyboard}
          disabled={state.isSaving}
        >
          {state.isSaving ? t('common.saving') : t('contextualRanking.saveDraft')}
        </Button>
        <Button
          ref={(element) => registerButton('undo', element)}
          disabled={!state.previous || state.isSaving}
          onClick={() => dispatch({ type: 'undo', message: t('contextualRanking.restored') })}
          onKeyDown={activateWithKeyboard}
        >
          {t('contextualRanking.undo')}
        </Button>
      </Stack>
    </Box>
  );
}
