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
  | { type: 'undo' } | { type: 'clearFocus' };

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
        ? { ...state, order: state.previous, previous: null, status: 'Orden anterior restaurado.', focusTarget: 'undo' }
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
  return (
    <Box component="li" draggable={!isExcluded} sx={{
      opacity: isExcluded ? 0.6 : 1, border: 1, borderColor: 'divider', borderRadius: 2, p: 1.25, bgcolor: 'background.paper',
    }}>
      <Stack direction="row" alignItems="center" spacing={1}>
        <Typography aria-label={isExcluded ? `${person.name}, excluido` : `Posición ${position}`} fontWeight={800} sx={{ minWidth: 28 }}>
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
        {person.verified && <Chip icon={<VerifiedIcon />} label="Verificado" size="small" color="success" variant="outlined" />}
      </Stack>
      <Stack direction="row" spacing={0.75} sx={{ mt: 1, ml: { sm: 5 } }}>
        <IconButton
          ref={(element) => registerButton(`up-${person.id}`, element)}
          aria-label={`Subir a ${person.name}`}
          onClick={() => onMove(person.id, -1)}
          onKeyDown={activateWithKeyboard}
          disabled={isFirst || isExcluded || actionDisabled}
        >
          ↑
        </IconButton>
        <IconButton
          ref={(element) => registerButton(`down-${person.id}`, element)}
          aria-label={`Bajar a ${person.name}`}
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
          {isExcluded ? 'Incluir' : 'No tengo suficiente información'}
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
    commit(order, `${person.name} ahora está en la posición ${nextIndex + 1}.`, `${delta < 0 ? 'up' : 'down'}-${id}`);
  };
  const toggleExcluded = (person: RankingPerson) => {
    const isExcluded = state.excluded.has(person.id);
    const message = isExcluded
      ? `${person.name} volvió a la comparación.`
      : `${person.name} se excluyó por falta de información.`;
    dispatch({ type: 'exclude', person, message });
  };
  const save = async () => {
    if (state.isSaving) return;
    dispatch({ type: 'saving', isSaving: true });
    try {
      await onSave?.(visible.map((person) => person.id));
      dispatch({ type: 'status', status: 'Borrador guardado. Puedes continuar más tarde.', focusTarget: 'save' });
    } catch {
      dispatch({ type: 'status', status: 'No se pudo guardar. Tu orden local se conserva; inténtalo de nuevo.', focusTarget: 'save' });
    } finally { dispatch({ type: 'saving', isSaving: false }); }
  };
  const dropAt = (index: number) => {
    const from = state.order.findIndex((person) => person.id === state.draggedId);
    if (from < 0 || from === index || !state.draggedId) return;
    const order = [...state.order];
    const [person] = order.splice(from, 1);
    if (!person) return;
    order.splice(index, 0, person);
    commit(order, `${person.name} ahora está en la posición ${index + 1}.`, `down-${person.id}`);
    dispatch({ type: 'drag', id: null });
  };

  return (
    <Box component="section" aria-labelledby="contextual-ranking-title" sx={{ maxWidth: 760, mx: 'auto', p: { xs: 2, sm: 3 } }}>
      <Typography id="contextual-ranking-title" variant="h5" fontWeight={800}>Ordena por: {category}</Typography>
      <Typography color="text.secondary" sx={{ mt: 0.5 }}>
        Arrastra o usa Subir/Bajar. Este resultado es una preferencia personal y no cambia la reputación pública.
      </Typography>
      <Typography color="text.secondary" variant="body2" sx={{ mt: 1 }}>
        Incluye sólo personas comparables. Marca «No tengo suficiente información» cuando corresponda.
      </Typography>
      <Box role="status" aria-live="polite" sx={{
        position: 'absolute', width: 1, height: 1, overflow: 'hidden', clip: 'rect(0 0 0 0)',
      }}>
        {state.status}
      </Box>
      {state.order.length === 0 ? (
        <Typography color="text.secondary" sx={{ mt: 3 }}>
          No hay personas para ordenar en esta interacción.
        </Typography>
      ) : (
        <Stack component="ol" aria-label={`Ranking de ${category}`} spacing={1.25} sx={{ listStyle: 'none', p: 0, mt: 2 }}>
          {state.order.map((person, index) => {
            const isExcluded = state.excluded.has(person.id);
            return (
              <Box
                key={person.id}
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
          onClick={() => dispatch({ type: 'undo' })}
          onKeyDown={activateWithKeyboard}
        >
          Deshacer
        </Button>
      </Stack>
    </Box>
  );
}
