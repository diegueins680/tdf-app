import { useEffect, useId, useMemo, useRef, useState } from 'react';
import type { ReactNode } from 'react';
import { useInfiniteQuery, useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { Alert, Avatar, Box, Button, CircularProgress, Dialog, DialogActions, DialogContent, DialogTitle,
  IconButton, Link, Menu, MenuItem, Select, Stack, TextField, Typography } from '@mui/material';
import MoreHorizIcon from '@mui/icons-material/MoreHoriz';
import { Link as RouterLink } from 'react-router-dom';
import { ApiError } from '../../api/client';
import { Interactions } from '../../api/interactions';
import type { InteractionCommand, InteractionComment, InteractionCommentContext, InteractionIdentity, InteractionPage, InteractionSort, InteractionSummary } from '../../api/interactions';
import { useSession } from '../../session/SessionContext';
import { getAnalyticsClient } from '../../analytics/posthog';
import { AccountControls } from './AccountControls';
import { DiscussionControls } from './DiscussionControls';
import { CommentComposer } from './CommentComposer';
import { createDiscussionCursorHistory, discussionWindowPages, commandAnalyticsEvents, discussionLink, getDisclosure, optimisticReaction, setDisclosure } from './model';

const unavailable = (error: unknown) => error instanceof ApiError && [401, 403, 404].includes(error.status);
const message = (error: unknown) => error instanceof ApiError && error.status === 409
  ? 'La conversación cambió. Actualiza y vuelve a intentarlo.'
  : error instanceof ApiError && error.status === 429 ? 'Espera un momento antes de volver a intentarlo.'
    : unavailable(error) ? 'La conversación no está disponible o tus permisos cambiaron.' : 'No se pudo completar la acción. Vuelve a intentarlo.';
const track = (event: string, kind: string) => getAnalyticsClient()?.capture(event, { platform: 'web', entity_kind: kind });
const unique = (comments: InteractionComment[]) => [...new Map(comments.map((comment) => [comment.id, comment])).values()];
type Run = (command: InteractionCommand, requestKey?: string) => Promise<void>;

function CommentBody({ comment }: { comment: InteractionComment }) {
  if (comment.state !== 'visible') return <Typography color="text.secondary">
    {comment.state === 'deleted' ? 'Comentario eliminado' : comment.state === 'hidden' || comment.state === 'removed' ? 'Comentario retirado' : 'Comentario no disponible'}
  </Typography>;
  const points = [...comment.body]; const nodes: ReactNode[] = []; let offset = 0;
  const plain = (text: string, key: string) => text.split(/(https?:\/\/[^\s<>]+)/g).map((part, index) =>
    /^https?:\/\//i.test(part) ? <Link key={`${key}-${index}`} href={part} target="_blank" rel="noopener noreferrer nofollow ugc">{part}</Link> : part);
  for (const mention of comment.mentions) {
    nodes.push(...plain(points.slice(offset, mention.start).join(''), String(offset)));
    nodes.push(<Link component={RouterLink} key={`mention-${mention.start}`} to={`/perfil/${mention.partyId}`}>{points.slice(mention.start, mention.end).join('')}</Link>);
    offset = mention.end;
  }
  nodes.push(...plain(points.slice(offset).join(''), String(offset)));
  return <Stack spacing={0.5}>
    {comment.legacyPresentation?.title && <Typography variant="subtitle2">{comment.legacyPresentation.title}</Typography>}
    <Typography sx={{ whiteSpace: 'pre-wrap', overflowWrap: 'anywhere' }}>{nodes}</Typography>
    {comment.legacyPresentation?.mediaUrls?.filter((url) => /^https?:\/\//i.test(url) || /^\/[^/]/.test(url)).map((url, index) => <Link key={`${index}-${url}`} href={url} target="_blank" rel="noopener noreferrer nofollow ugc">Archivo adjunto {index + 1}</Link>)}
  </Stack>;
}

function CommentCard({ comment, summary, run, onReply, focused, refresh }: {
  comment: InteractionComment; summary: InteractionSummary; run: Run; onReply: (comment: InteractionComment) => void; focused: boolean; refresh: () => Promise<void>;
}) {
  const [anchor, setAnchor] = useState<HTMLElement | null>(null);
  const [editing, setEditing] = useState(false);
  const [action, setAction] = useState<'delete' | 'hide' | 'remove' | 'report' | 'block' | null>(null);
  const [reason, setReason] = useState(''); const [error, setError] = useState(''); const [pending, setPending] = useState(false);
  const [copied, setCopied] = useState(false);
  const article = useRef<HTMLElement>(null); const menuButton = useRef<HTMLButtonElement>(null);
  const { session } = useSession(); const actionKey = useRef<string | null>(null);
  useEffect(() => {
    if (!focused) return;
    const timer = window.setTimeout(() => {
      article.current?.scrollIntoView?.({ block: 'center', behavior: 'auto' }); article.current?.focus({ preventScroll: true });
    }, 0);
    return () => window.clearTimeout(timer);
  }, [focused]);
  const choose = (next: typeof action) => { setAnchor(null); setError(''); setReason(''); actionKey.current = null; setAction(next); };
  return <Box component="article" ref={article} tabIndex={-1} aria-label={`Comentario de ${comment.author?.displayName ?? 'usuario'}`}
    id={`comment-${comment.id}`} sx={{ py: 1.5, px: 1, borderRadius: 2, outlineOffset: 2,
      ...(focused ? { animation: 'discussion-focus 4s ease-out', '@keyframes discussion-focus': { from: { backgroundColor: 'rgba(124,58,237,.16)' }, to: { backgroundColor: 'transparent' } } } : {}) }}>
    <Stack direction="row" spacing={1} alignItems="center">
      {comment.author && <Avatar src={comment.author.avatarUrl ?? undefined} alt="" sx={{ width: 28, height: 28 }} />}
      <Typography variant="subtitle2">{comment.author?.displayName ?? 'Comentario'}</Typography>
      <Typography variant="caption" color="text.secondary" component="time" dateTime={comment.createdAt}>
        {new Date(comment.createdAt).toLocaleString()}{comment.editedAt ? ' · editado' : ''}
      </Typography>
      <Box sx={{ flex: 1 }} />
      <IconButton ref={menuButton} aria-label="Opciones del comentario" aria-haspopup="menu" aria-expanded={Boolean(anchor)}
        onClick={(event) => setAnchor(event.currentTarget)} sx={{ minWidth: 44, minHeight: 44 }}><MoreHorizIcon /></IconButton>
    </Stack>
    {comment.parentId && <Link component={RouterLink} to={discussionLink('comment', comment.parentId)} variant="caption">En respuesta a otro comentario</Link>}
    {editing ? <CommentComposer focusOnMount targetId={summary.id} initialBody={comment.body} initialMentions={comment.mentions} label="Editar comentario"
      onCancel={() => { setEditing(false); menuButton.current?.focus(); }} onSave={async (body, mentions, key) => {
        await run({ operation: 'comment.edit', commentId: comment.id, expectedVersion: comment.version, body, mentions }, key);
        setEditing(false); menuButton.current?.focus();
      }} /> : <CommentBody comment={comment} />}
    {summary.canComment && ['visible', 'deleted'].includes(comment.state) && <Button size="small" onClick={() => onReply(comment)} sx={{ minHeight: 44 }}>Responder</Button>}
    <Typography role="status" variant="caption">{copied ? 'Enlace copiado' : ''}</Typography>
    <Menu anchorEl={anchor} open={Boolean(anchor)} onClose={() => setAnchor(null)}>
      <MenuItem onClick={() => {
        setAnchor(null); void navigator.clipboard.writeText(`${window.location.origin}${discussionLink('comment', comment.id)}`)
          .then(() => setCopied(true)).catch(() => setError('No se pudo copiar el enlace.'));
      }}>Copiar enlace</MenuItem>
      {comment.canEdit && <MenuItem onClick={() => { setAnchor(null); setEditing(true); }}>Editar</MenuItem>}
      {comment.canDelete && <MenuItem onClick={() => choose('delete')}>Eliminar mi comentario</MenuItem>}
      {session && comment.state === 'visible' && <MenuItem onClick={() => choose('report')}>Reportar</MenuItem>}
      {summary.canManage && comment.state === 'visible' && <MenuItem onClick={() => choose('hide')}>Ocultar en mi contenido</MenuItem>}
      {summary.canModerate && comment.state === 'visible' && <MenuItem onClick={() => choose('remove')}>Retirar como administrador</MenuItem>}
      {session && comment.author && comment.author.id !== session.partyId && <MenuItem onClick={() => choose('block')}>Bloquear usuario</MenuItem>}
    </Menu>
    {error && !action && <Alert severity="error">{error}</Alert>}
    <Dialog open={action !== null} onClose={() => { if (!pending) setAction(null); }} aria-labelledby={`action-${comment.id}`}>
      <DialogTitle id={`action-${comment.id}`}>{action === 'delete' ? 'Eliminar mi comentario' : action === 'block' ? 'Bloquear usuario' : action === 'report' ? 'Reportar comentario' : 'Moderar comentario'}</DialogTitle>
      <DialogContent>
        <Typography>{action === 'delete' ? 'El texto se eliminará. Las respuestas se conservarán.' : action === 'block' ? 'El bloqueo se aplica a la interacción entre ambas cuentas. Desbloquear no restaura conexiones anteriores.' : 'Explica brevemente el motivo.'}</Typography>
        {action && !['delete', 'block'].includes(action) && <TextField label="Motivo" fullWidth multiline value={reason} onChange={(event) => { setReason(event.target.value); actionKey.current = null; }} inputProps={{ maxLength: 1000 }} sx={{ mt: 2 }} />}
        {error && <Alert severity="error">{error}</Alert>}
      </DialogContent>
      <DialogActions><Button disabled={pending} onClick={() => setAction(null)}>Cancelar</Button><Button disabled={pending || (!!action && !['delete', 'block'].includes(action) && !reason.trim())} onClick={() => {
        if (!action) return; setPending(true); setError(''); actionKey.current ??= crypto.randomUUID();
        const task = action === 'block' && comment.author
          ? Interactions.blockState(comment.author.id).then((state) => state.blocked ? state : Interactions.block(state.partyId, true, state.version, actionKey.current!)).then(() => refresh())
          : run(action === 'report' ? { operation: 'comment.report', commentId: comment.id, reason }
            : action === 'delete' ? { operation: 'comment.delete', commentId: comment.id, expectedVersion: comment.version }
              : { operation: action === 'hide' ? 'comment.hide' : 'comment.remove', commentId: comment.id, expectedVersion: comment.version, reason }, actionKey.current);
        void task.then(() => { setAction(null); article.current?.focus(); }).catch((err: unknown) => setError(message(err))).finally(() => setPending(false));
      }}>{pending ? 'Guardando…' : 'Confirmar'}</Button></DialogActions>
    </Dialog>
  </Box>;
}

function Thread({ root, identity, summary, run, scope, authenticated, context, focusId, refresh }: {
  root: InteractionComment; identity: InteractionIdentity; summary: InteractionSummary; run: Run; scope: string; authenticated: boolean;
  context?: InteractionCommentContext; focusId?: string; refresh: () => Promise<void>;
}) {
  const key = `${scope}:thread:${root.id}`;
  const [expanded, setExpanded] = useState(() => Boolean(context) || getDisclosure(key));
  const [reply, setReply] = useState<InteractionComment | null>(null);
  const toggleButton = useRef<HTMLButtonElement>(null); const repliesId = useId();
  const replyCursors = useMemo(createDiscussionCursorHistory, [scope, identity.kind, identity.entityKey, root.id]);
  const replies = useInfiniteQuery({ queryKey: ['interactions', scope, identity.kind, identity.entityKey, 'replies', root.id],
    initialPageParam: '', maxPages: discussionWindowPages,
    getPreviousPageParam: (_page, _pages, cursor) => replyCursors.previous(cursor),
    queryFn: async ({ pageParam, signal }) => { const page = await Interactions.comments(identity, authenticated, 'oldest', root.id, pageParam || undefined, signal); replyCursors.remember(pageParam, page.nextCursor); return page; },
    getNextPageParam: (page: InteractionPage) => page.nextCursor ?? undefined, enabled: expanded, retry: false, refetchInterval: 30000,
  });
  useEffect(() => { if (context) setExpanded(true); }, [context]);
  const items = unique([...(context ? [context.parent, context.comment, ...context.surrounding].filter((c): c is InteractionComment => !!c && c.id !== root.id) : []),
    ...(replies.data?.pages.flatMap((page) => page.items) ?? [])]).sort((a, b) => a.createdAt.localeCompare(b.createdAt) || a.id.localeCompare(b.id));
  const openReply = (comment: InteractionComment) => { setReply(comment); setExpanded(true); setDisclosure(key, true); };
  return <Box>
    <CommentCard comment={root} summary={summary} run={run} onReply={openReply} focused={focusId === root.id} refresh={refresh} />
    {((root.replyCount ?? 0) > 0 || expanded) && <Button ref={toggleButton} aria-expanded={expanded} aria-controls={repliesId} onClick={() => {
      setExpanded(!expanded); setDisclosure(key, !expanded); track(expanded ? 'thread_collapsed' : 'thread_expanded', identity.kind);
      if (expanded) { setReply(null); toggleButton.current?.focus(); }
    }}>{expanded ? 'Ocultar respuestas' : `Ver ${root.replyCount ?? 0} respuestas`}</Button>}
    {expanded && <Box id={repliesId} sx={{ ml: { xs: 1, sm: 3 } }}>
      {replies.hasPreviousPage && <Button disabled={replies.isFetchingPreviousPage} onClick={() => void replies.fetchPreviousPage()}>Ver respuestas anteriores</Button>}
      {replies.isPending && <Typography role="status">Cargando respuestas…</Typography>}
      {replies.isError ? <Alert severity="error" action={<Button onClick={() => void replies.refetch()}>Reintentar</Button>}>{message(replies.error)}</Alert>
        : items.map((comment) => <CommentCard key={comment.id} comment={comment} summary={summary} run={run} onReply={openReply} focused={focusId === comment.id} refresh={refresh} />)}
      {replies.hasNextPage && <Button disabled={replies.isFetchingNextPage} onClick={() => void replies.fetchNextPage()}>Ver más respuestas</Button>}
      {reply && summary.canComment && <Box sx={{ pt: 1 }}><CommentComposer focusOnMount key={reply.id} targetId={summary.id}
        label={`Responder a ${reply.author?.displayName ?? 'este comentario'}`} onCancel={() => { setReply(null); toggleButton.current?.focus(); }}
        onSave={async (body, mentions, requestKey) => { await run({ operation: 'comment.create', parentId: reply.id, body, mentions }, requestKey); setReply(null); }} /></Box>}
    </Box>}
  </Box>;
}

type PanelProps = InteractionIdentity & { focusCommentId?: string; initiallyExpanded?: boolean };
export function InteractionPanel(props: PanelProps) {
  const container = useRef<HTMLDivElement>(null);
  const [active, setActive] = useState(Boolean(props.focusCommentId ?? props.initiallyExpanded));
  useEffect(() => {
    if (typeof IntersectionObserver === 'undefined') { setActive(true); return; }
    const observer = new IntersectionObserver(([entry]) => setActive(entry?.isIntersecting ?? false), { rootMargin: '100px' });
    if (container.current) observer.observe(container.current);
    return () => observer.disconnect();
  }, []);
  return <div ref={container}><InteractionPanelContent {...props} active={active} /></div>;
}
function InteractionPanelContent({ kind, entityKey, focusCommentId, initiallyExpanded = false, active }: PanelProps & { active: boolean }) {
  const { session } = useSession(); const authenticated = Boolean(session); const scope = session ? `account:${session.partyId ?? session.username}` : 'anonymous';
  const identity = { kind, entityKey }; const client = useQueryClient(); const sectionId = useId();
  const disclosureKey = `${scope}:${kind}:${entityKey}`;
  const [expanded, setExpanded] = useState(() => initiallyExpanded || Boolean(focusCommentId) || getDisclosure(disclosureKey));
  const [sort, setSort] = useState<InteractionSort>('newest'); const [error, setError] = useState('');
  const attemptedCommands = useRef(new Map<string, InteractionCommand>());
  const [reactorsOpen, setReactorsOpen] = useState(false);
  const summaryKey = ['interactions', scope, kind, entityKey, 'summary'];
  const summary = useQuery({ queryKey: summaryKey, queryFn: ({ signal }) => Interactions.summary(identity, authenticated, signal), enabled: active || expanded, retry: false, refetchInterval: active ? 30000 : false });
  const commentCursors = useMemo(createDiscussionCursorHistory, [scope, kind, entityKey, sort]);
  const comments = useInfiniteQuery({ queryKey: ['interactions', scope, kind, entityKey, 'comments', sort], initialPageParam: '', maxPages: discussionWindowPages,
    getPreviousPageParam: (_page, _pages, cursor) => commentCursors.previous(cursor),
    queryFn: async ({ pageParam, signal }) => { const page = await Interactions.comments(identity, authenticated, sort, undefined, pageParam || undefined, signal); commentCursors.remember(pageParam, page.nextCursor); return page; },
    getNextPageParam: (page: InteractionPage) => page.nextCursor ?? undefined, enabled: expanded && Boolean(summary.data), retry: false, refetchInterval: 30000 });
  const context = useQuery({ queryKey: ['interactions', scope, kind, entityKey, 'context', focusCommentId],
    queryFn: ({ signal }) => Interactions.context(identity, authenticated, focusCommentId!, signal), enabled: Boolean(focusCommentId && summary.data), retry: false });
  const reactors = useInfiniteQuery({ queryKey: ['interactions', scope, kind, entityKey, 'reactors'], initialPageParam: undefined as number | undefined,
    queryFn: ({ pageParam, signal }) => Interactions.reactors(identity, authenticated, pageParam, signal),
    getNextPageParam: (page) => page.nextCursor ?? undefined, enabled: reactorsOpen && authenticated, retry: false });
  const refresh = async () => { await client.invalidateQueries({ queryKey: ['interactions'] }); };
  const mutation = useMutation({ mutationFn: ({ command, requestKey }: { command: InteractionCommand; requestKey: string }) => Interactions.command(summary.data!.id, command, requestKey),
    onSuccess: (_result, { command }) => commandAnalyticsEvents(command).forEach((event) => track(event, kind)), onSettled: refresh,
  });
  const run: Run = async (command, requestKey = crypto.randomUUID()) => {
    const attempted = attemptedCommands.current;
    if (!attempted.has(requestKey)) attempted.set(requestKey, command);
    if (attempted.size > 16) attempted.delete(attempted.keys().next().value!);
    await mutation.mutateAsync({ command: attempted.get(requestKey)!, requestKey });
    attempted.delete(requestKey);
  };
  const reaction = useMutation({ mutationFn: (id: string | null) => Interactions.command(summary.data!.id, { operation: 'reaction.set', reactionTypeId: id }, crypto.randomUUID()),
    onMutate: async (id) => { setError(''); await client.cancelQueries({ queryKey: summaryKey }); const previous = client.getQueryData<InteractionSummary>(summaryKey);
      if (previous) client.setQueryData(summaryKey, optimisticReaction(previous, id)); return previous; },
    onError: (err, _id, previous) => { if (previous) client.setQueryData(summaryKey, previous); setError(message(err)); },
    onSuccess: (_result, id, previous) => track(id === null ? 'reaction_removed' : previous?.myReactionTypeId ? 'reaction_changed' : 'reaction_added', kind), onSettled: refresh,
  });
  useEffect(() => { if (focusCommentId && context.data) { setExpanded(true); track('comment_deep_link_opened', kind); } }, [focusCommentId, context.data, kind]);
  if (summary.isPending) return <Typography role="status" variant="caption">Cargando conversación…</Typography>;
  if (summary.isError) return unavailable(summary.error) ? null : <Alert severity="warning" action={<Button onClick={() => void summary.refetch()}>Reintentar</Button>}>No se pudo cargar la conversación.</Alert>;
  const data = summary.data; if (!data) return null;
  const roots = unique([...(context.data ? [context.data.root] : []), ...(comments.data?.pages.flatMap((page) => page.items) ?? [])]);
  const totalReactions = data.reactions.reduce((sum, item) => sum + item.count, 0);
  return <Stack spacing={1} component="section" aria-label="Reacciones y conversación" sx={{ mt: 1, '& button': { minHeight: 44 } }}>
    <Stack direction="row" alignItems="center" flexWrap="wrap" gap={0.5}>
      {data.reactable && data.reactions.filter((item) => item.selectable || item.count > 0).map((item) => <Button key={item.id}
        aria-label={`${item.label}: ${item.count}`} aria-pressed={data.myReactionTypeId === item.id}
        variant={data.myReactionTypeId === item.id ? 'outlined' : 'text'} disabled={!data.canReact || reaction.isPending || (!item.selectable && data.myReactionTypeId !== item.id)}
        onClick={() => reaction.mutate(data.myReactionTypeId === item.id ? null : item.id)}>{item.emoji}{item.count > 0 ? ` ${item.count}` : ''}</Button>)}
      {authenticated && totalReactions > 0 && <Button onClick={() => setReactorsOpen(true)}>Ver reacciones</Button>}
      {data.commentable && <Button aria-expanded={expanded} aria-controls={sectionId} onClick={() => {
        setExpanded(!expanded); setDisclosure(disclosureKey, !expanded); track(expanded ? 'thread_collapsed' : 'thread_expanded', kind);
      }}>{expanded ? 'Ocultar comentarios' : data.commentCount > 0 ? `Ver los ${data.commentCount} comentarios` : 'Comentar'}</Button>}
    </Stack>
    {error && <Alert severity="error">{error}</Alert>}
    {expanded && <Stack spacing={1} id={sectionId}>
      <Stack direction="row" spacing={1} flexWrap="wrap">
        <Select size="small" value={sort} inputProps={{ 'aria-label': 'Ordenar comentarios' }} onChange={(event) => setSort(event.target.value as InteractionSort)}>
          <MenuItem value="newest">Más recientes</MenuItem><MenuItem value="oldest">Más antiguos</MenuItem>
        </Select>
        {authenticated && <Select size="small" value={data.subscription} disabled={mutation.isPending} inputProps={{ 'aria-label': 'Notificaciones de esta conversación' }}
          onChange={(event) => void run({ operation: 'subscription.set', mode: event.target.value as 'all' | 'participating' | 'muted' }).catch((err: unknown) => setError(message(err)))}>
          <MenuItem value="participating">Respuestas y menciones</MenuItem><MenuItem value="all">Toda la conversación</MenuItem><MenuItem value="muted">Silenciar</MenuItem>
        </Select>}
      </Stack>
      {(data.canManage || data.canModerate) && <DiscussionControls summary={data} scope={scope} run={run} />}
      {authenticated && <AccountControls scope={scope} canModerate={data.canModerate} />}
      {!authenticated && <Typography><Link component={RouterLink} to="/login">Inicia sesión</Link> para participar.</Typography>}
      {data.commentPolicy === 'off' ? <Typography>Los comentarios están desactivados.</Typography> : !data.canComment && authenticated ? <Typography>No tienes permiso para comentar en esta publicación.</Typography> : null}
      {data.canComment && <CommentComposer targetId={data.id} onSave={(body, mentions, requestKey) => run({ operation: 'comment.create', body, mentions }, requestKey)} />}
      {context.isError && <Alert severity="info">El comentario enlazado ya no está disponible.</Alert>}
      {comments.isPending && <Typography role="status">Cargando comentarios…</Typography>}
      {comments.hasPreviousPage && <Button disabled={comments.isFetchingPreviousPage} onClick={() => void comments.fetchPreviousPage()}>Ver comentarios anteriores</Button>}
      {comments.isError ? <Alert severity="error" action={<Button onClick={() => void comments.refetch()}>Reintentar</Button>}>{message(comments.error)}</Alert>
        : roots.map((root) => <Thread key={root.id} root={root} identity={identity} summary={data} run={run} scope={scope} authenticated={authenticated}
          context={context.data?.root.id === root.id ? context.data : undefined} focusId={focusCommentId} refresh={refresh} />)}
      {!comments.isPending && !comments.isError && roots.length === 0 && <Typography color="text.secondary">Todavía no hay comentarios.</Typography>}
      {comments.hasNextPage && <Button disabled={comments.isFetchingNextPage} onClick={() => void comments.fetchNextPage()}>Ver más comentarios</Button>}
    </Stack>}
    <Dialog open={reactorsOpen} onClose={() => setReactorsOpen(false)} aria-labelledby={`${sectionId}-reactions`}>
      <DialogTitle id={`${sectionId}-reactions`}>Reacciones</DialogTitle><DialogContent>
        {reactors.isPending && <CircularProgress aria-label="Cargando reacciones" />}
        {reactors.isError && <Alert severity="error">{message(reactors.error)}</Alert>}
        {reactors.data?.pages.flatMap((page) => page.items).map((item) => <Stack key={item.author.id} direction="row" spacing={1} sx={{ py: 1 }}>
          <Avatar src={item.author.avatarUrl ?? undefined} alt="" /><Typography>{item.author.displayName} {data.reactions.find((r) => r.id === item.reactionTypeId)?.emoji}</Typography>
        </Stack>)}
        <Typography variant="caption">Las preferencias de privacidad pueden limitar qué personas aparecen.</Typography>
        {reactors.hasNextPage && <Button disabled={reactors.isFetchingNextPage} onClick={() => void reactors.fetchNextPage()}>Ver más</Button>}
      </DialogContent><DialogActions><Button onClick={() => setReactorsOpen(false)}>Cerrar</Button></DialogActions>
    </Dialog>
  </Stack>;
}
