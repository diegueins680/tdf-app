import { useEffect, useRef, useState } from 'react';
import { Alert, Button, Stack, TextField } from '@mui/material';
import { UserSelector } from '../../components/party-selector/PartySelector';
import type { InteractionMention } from '../../api/interactions';
import { appendMention, mentionQuery, reconcileMentions } from './model';

interface Props {
  targetId: string;
  initialBody?: string;
  initialMentions?: InteractionMention[];
  label?: string;
  focusOnMount?: boolean;
  onSave: (body: string, mentions: InteractionMention[], requestKey: string) => Promise<void>;
  onCancel?: () => void;
}
export function CommentComposer({ targetId, initialBody = '', initialMentions = [], label = 'Escribe un comentario', focusOnMount = false, onSave, onCancel }: Props) {
  const [body, setBody] = useState(initialBody);
  const [mentions, setMentions] = useState(initialMentions);
  const [mentionOpen, setMentionOpen] = useState(false);
  const [pending, setPending] = useState(false);
  const [error, setError] = useState('');
  const input = useRef<HTMLTextAreaElement>(null);
  useEffect(() => { if (focusOnMount) input.current?.focus(); }, [focusOnMount]);
  // Keep the key across uncertain network failures. Editing starts a new command.
  const request = useRef<string | null>(null);
  const update = (next: string) => {
    setMentions(reconcileMentions(body, next, mentions)); setBody(next); request.current = null; if (mentionQuery(next) !== undefined) setMentionOpen(true);
  };
  return <Stack component="form" spacing={1} onSubmit={(event) => {
    event.preventDefault(); if (pending || !body.trim()) return;
    setPending(true); setError(''); request.current ??= crypto.randomUUID();
    void onSave(body, mentions, request.current).then(() => {
      setBody(''); setMentions([]); request.current = null; input.current?.focus();
    }).catch(() => setError('No se pudo publicar. Tu texto sigue aquí; vuelve a intentarlo.'))
      .finally(() => setPending(false));
  }}>
    <TextField inputRef={input} label={label} multiline minRows={2} maxRows={8} value={body} disabled={pending}
      onChange={(event) => update(event.target.value)}
      helperText={`${[...body].length}/4096 · Texto, emojis y enlaces`}
      error={[...body].length > 4096} inputProps={{ 'aria-label': label }} />
    {mentionOpen && <UserSelector value={null} onChange={(person) => {
      if (!person) return;
      const next = appendMention(body, mentions, person.partyId, person.username ?? person.displayName);
      setBody(next.body); setMentions(next.mentions);
      request.current = null; setMentionOpen(false); input.current?.focus();
    }} field={{ label: 'Mencionar a una persona' }} search={{ context: 'interaction_mention', scopeId: targetId, initialQuery: mentionQuery(body) }} />}
    {error && <Alert severity="error" role="alert">{error}</Alert>}
    <Stack direction="row" spacing={1}>
      <Button type="button" disabled={pending || mentions.length >= 20} aria-expanded={mentionOpen} onClick={() => setMentionOpen(!mentionOpen)}>@ Mencionar</Button>
      <Button type="submit" variant="contained" disabled={pending || !body.trim() || [...body].length > 4096}>{pending ? 'Publicando…' : 'Publicar'}</Button>
      {onCancel && <Button disabled={pending} onClick={onCancel}>Cancelar</Button>}
    </Stack>
  </Stack>;
}
