import { useMemo, useState } from 'react';
import DragIndicatorIcon from '@mui/icons-material/DragIndicator';
import VerifiedIcon from '@mui/icons-material/Verified';
import { Alert, Avatar, Box, Button, Chip, IconButton, Stack, Typography } from '@mui/material';

export type RankingPerson = {
  id: string;
  name: string;
  professionalName?: string;
  role: string;
  city?: string;
  interaction: string;
  verified: boolean;
  avatarUrl?: string;
};

type Props = {
  category: string;
  people: RankingPerson[];
  onSave?: (orderedIds: string[]) => Promise<void> | void;
};

const initials = (name: string) => name.split(/\s+/).filter(Boolean).slice(0, 2).map((part) => part[0]).join('').toUpperCase();

/**
 * Story-ready accessible prototype. Native drag is progressive enhancement;
 * the position controls are the canonical keyboard and touch alternative.
 */
export default function ContextualRankingPrototype({ category, people, onSave }: Props) {
  const [order, setOrder] = useState(() => [...people].sort(() => Math.random() - 0.5));
  const [previous, setPrevious] = useState<RankingPerson[] | null>(null);
  const [draggedId, setDraggedId] = useState<string | null>(null);
  const [excluded, setExcluded] = useState<Set<string>>(new Set());
  const [status, setStatus] = useState('');
  const visible = useMemo(() => order.filter((person) => !excluded.has(person.id)), [order, excluded]);

  const commit = (next: RankingPerson[], message: string) => {
    setPrevious(order);
    setOrder(next);
    setStatus(message);
  };
  const move = (id: string, delta: number) => {
    const index = order.findIndex((person) => person.id === id);
    const nextIndex = index + delta;
    if (index < 0 || nextIndex < 0 || nextIndex >= order.length) return;
    const next = [...order];
    [next[index], next[nextIndex]] = [next[nextIndex], next[index]];
    commit(next, `${order[index].name} ahora está en la posición ${nextIndex + 1}.`);
  };
  const toggleExcluded = (person: RankingPerson) => {
    const next = new Set(excluded);
    if (next.has(person.id)) next.delete(person.id); else next.add(person.id);
    setExcluded(next);
    setStatus(next.has(person.id) ? `${person.name} se excluyó por falta de información.` : `${person.name} volvió a la comparación.`);
  };
  const save = async () => {
    try {
      await onSave?.(visible.map((person) => person.id));
      setStatus('Borrador guardado. Puedes continuar más tarde.');
    } catch {
      setStatus('No se pudo guardar. Tu orden local se conserva; inténtalo de nuevo.');
    }
  };

  return (
    <Box component="section" aria-labelledby="contextual-ranking-title" sx={{ maxWidth: 760, mx: 'auto', p: { xs: 2, sm: 3 } }}>
      <Typography id="contextual-ranking-title" variant="h5" fontWeight={800}>Ordena por: {category}</Typography>
      <Typography color="text.secondary" sx={{ mt: 0.5 }}>Arrastra o usa Subir/Bajar. Este resultado es una preferencia personal y no cambia la reputación pública.</Typography>
      <Alert severity="info" sx={{ mt: 2 }}>Incluye sólo personas comparables de esta interacción. Puedes marcar «No tengo suficiente información».</Alert>
      <Box role="status" aria-live="polite" sx={{ position: 'absolute', width: 1, height: 1, overflow: 'hidden', clip: 'rect(0 0 0 0)' }}>{status}</Box>
      <Stack component="ol" aria-label={`Ranking de ${category}`} spacing={1.25} sx={{ listStyle: 'none', p: 0, mt: 2 }}>
        {order.map((person, index) => {
          const isExcluded = excluded.has(person.id);
          return (
            <Box component="li" key={person.id} draggable={!isExcluded}
              onDragStart={() => setDraggedId(person.id)}
              onDragOver={(event) => event.preventDefault()}
              onDrop={() => {
                const from = order.findIndex((item) => item.id === draggedId);
                if (from < 0 || from === index || !draggedId) return;
                const next = [...order]; const [item] = next.splice(from, 1); next.splice(index, 0, item);
                commit(next, `${item.name} ahora está en la posición ${index + 1}.`); setDraggedId(null);
              }}
              sx={{ opacity: isExcluded ? 0.6 : 1, border: 1, borderColor: 'divider', borderRadius: 2, p: 1.25, bgcolor: 'background.paper' }}>
              <Stack direction="row" alignItems="center" spacing={1}>
                <Typography aria-label={isExcluded ? `${person.name}, excluido` : `Posición ${visible.findIndex((item) => item.id === person.id) + 1}`} fontWeight={800} sx={{ minWidth: 28 }}>{isExcluded ? '—' : visible.findIndex((item) => item.id === person.id) + 1}</Typography>
                <DragIndicatorIcon aria-hidden="true" color="action" />
                <Avatar src={person.avatarUrl} alt="">{initials(person.name)}</Avatar>
                <Box sx={{ flex: 1, minWidth: 0 }}>
                  <Typography fontWeight={700}>{person.name}{person.professionalName ? ` · ${person.professionalName}` : ''}</Typography>
                  <Typography variant="body2" color="text.secondary">{person.role}{person.city ? ` · ${person.city}` : ''} · {person.interaction}</Typography>
                </Box>
                {person.verified && <Chip icon={<VerifiedIcon />} label="Verificado" size="small" color="success" variant="outlined" />}
              </Stack>
              <Stack direction="row" spacing={0.75} sx={{ mt: 1, ml: { sm: 5 } }}>
                <IconButton aria-label={`Subir a ${person.name}`} onClick={() => move(person.id, -1)} disabled={index === 0 || isExcluded}>↑</IconButton>
                <IconButton aria-label={`Bajar a ${person.name}`} onClick={() => move(person.id, 1)} disabled={index === order.length - 1 || isExcluded}>↓</IconButton>
                <Button size="small" onClick={() => toggleExcluded(person)}>{isExcluded ? 'Incluir' : 'No tengo suficiente información'}</Button>
              </Stack>
            </Box>
          );
        })}
      </Stack>
      <Stack direction="row" spacing={1} sx={{ mt: 2 }}>
        <Button variant="contained" onClick={() => { void save(); }}>Guardar borrador</Button>
        <Button disabled={!previous} onClick={() => { if (previous) { setOrder(previous); setPrevious(null); setStatus('Orden anterior restaurado.'); } }}>Deshacer</Button>
      </Stack>
    </Box>
  );
}
