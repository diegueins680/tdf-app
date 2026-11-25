import { useEffect, useState } from 'react';
import { useQuery } from '@tanstack/react-query';
import { Alert, Button, Stack, TextField, Typography } from '@mui/material';
import { Directory } from '../../api/directory';
import { newUploadKey } from '../../api/uploadFailure';
import { uploadToDrive } from '../../api/drive';

interface Post { id: string; caption: string; images: string[] }
interface Photo { file: File; key: string; preview: string }
const types = new Set(['image/jpeg', 'image/png', 'image/webp', 'image/gif']);

function FeedPhoto({ slug, file, label }: { slug: string; file: string; label: string }) {
  const photo = useQuery({ queryKey: ['studio-feed-image', slug, file], queryFn: () => Directory.feedImage(slug, file), gcTime: 0 });
  return photo.data ? <img src={photo.data} alt={label} style={{ maxWidth: '100%', maxHeight: 440 }} /> : null;
}

export function StudioFeed({ profile, slug, name, signedIn, posts, refresh }: {
  profile: string; slug: string; name: string; signedIn: boolean; posts: Post[]; refresh: () => Promise<unknown>;
}) {
  const managed = useQuery({ queryKey: ['studio-feed-permissions', profile], queryFn: Directory.managedProfiles, enabled: signedIn });
  const permissions = managed.data?.find(p => p.id === profile)?.capabilities;
  const editor = signedIn && permissions?.['edit'] && permissions['publish'];
  const [caption, setCaption] = useState('');
  const [photos, setPhotos] = useState<Photo[]>([]);
  const [postId, setPostId] = useState(() => crypto.randomUUID());
  const [preview, setPreview] = useState(false);
  const [busy, setBusy] = useState(false);
  const [message, setMessage] = useState('');
  const [editing, setEditing] = useState<Post | null>(null);
  useEffect(() => () => photos.forEach(photo => URL.revokeObjectURL(photo.preview)), [photos]);
  async function perform(action: () => Promise<void>) {
    setBusy(true);
    setMessage('');
    try { await action(); await refresh(); }
    catch (error) { setMessage(error instanceof Error ? error.message : 'No se pudo guardar. Puedes reintentar.'); }
    finally { setBusy(false); }
  }
  async function publish() {
    const images: string[] = [];
    for (const photo of photos) {
      const stored = await uploadToDrive(photo.file, { destination: 'tdf', visibility: 'private', idempotencyKey: photo.key });
      images.push(stored.id);
    }
    await Directory.publishFeed(profile, postId, caption.trim(), images);
    setCaption(''); setPhotos([]); setPreview(false); setPostId(crypto.randomUUID());
  }
  return <Stack component="section" spacing={2} aria-label="Feed">
    <Typography variant="h5">Feed</Typography>
    {editor && <fieldset disabled={busy}><Stack spacing={1}>
      <Typography>Publicar como {name}</Typography>
      <TextField label="Texto de la publicación" value={caption} disabled={preview} onChange={e => setCaption(e.target.value)} inputProps={{ maxLength: 2000 }} />
      {!preview && <input type="file" aria-label="Fotos en orden" multiple accept={Array.from(types).join(',')} onChange={event => {
        const files = Array.from(event.target.files ?? []);
        if (files.length > 12 || files.some(f => !types.has(f.type) || !f.size || f.size > 8388608)) {
          setMessage('Elige hasta 12 imágenes JPG, PNG, GIF o WebP de hasta 8 MiB cada una.'); return;
        }
        setMessage('');
        setPhotos(files.map(file => ({ file, key: newUploadKey(), preview: URL.createObjectURL(file) })));
      }} />}
      {photos.map((photo, i) => <div key={photo.key}><img width={100} src={photo.preview} alt={`Foto ${i + 1}`} /> {i + 1}. {photo.file.name}
        {!preview && i > 0 && <Button onClick={() => {
          const reordered = [...photos]; reordered.splice(i, 1); reordered.splice(i - 1, 0, photo);
          setPhotos(reordered.map(p => ({ ...p, preview: URL.createObjectURL(p.file) })));
        }}>Mover antes</Button>}</div>)}
      {preview ? <><Button onClick={() => setPreview(false)}>Volver a editar</Button><Button onClick={() => void perform(publish)}>Confirmar publicación</Button></>
        : <Button disabled={!caption.trim() || !photos.length} onClick={() => setPreview(true)}>Vista previa</Button>}
    </Stack></fieldset>}
    {message && <Alert severity="error">{message}</Alert>}
    {!posts.length && <Typography>Aún no hay publicaciones.</Typography>}
    {posts.map(post => <Stack component="article" key={post.id} spacing={1}>
      <Typography>{post.caption}</Typography>
      {post.images.map((file,i) => <FeedPhoto key={file} slug={slug} file={file} label={`Foto ${i + 1} de ${name}`} />)}
      {editor && (editing?.id === post.id ? <>
        <TextField label="Editar texto" value={editing.caption} onChange={e => setEditing({ ...editing, caption: e.target.value })} />
        <Button disabled={busy || !editing.caption.trim()} onClick={() => void perform(async () => { await Directory.editFeed(profile, post.id, editing.caption, true); setEditing(null); })}>Guardar texto</Button>
        <Button onClick={() => setEditing(null)}>Cancelar</Button></> : <Button onClick={() => setEditing(post)}>Editar publicación</Button>)}
      {editor && <Button disabled={busy} onClick={() => void perform(async () => { await Directory.editFeed(profile, post.id, post.caption, false); })}>Retirar publicación</Button>}
    </Stack>)}
  </Stack>;
}
