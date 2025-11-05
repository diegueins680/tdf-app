
import { useState } from "react";
import { Dialog, DialogTitle, DialogContent, DialogActions, TextField, Button, Chip, Stack } from "@mui/material";
import dayjs from "dayjs";

type Props = {
  open: boolean;
  onClose: () => void;
  projectId: number;
  onOk: () => void;
  defaults?: Partial<Meta>;
  api: { post: (url: string, body: any) => Promise<any> };
};

type Meta = {
  ytTitle: string;
  ytDescription: string;
  ytTags: string[];
  ytPlaylistId: string;
  ytPublishAt: string;      // ISO with timezone -05:00
  ytVideoFileId: string;
  ytThumbFileId?: string;
};

const DEFAULT_TAGS = [
  "TDF Sessions","TDF Records","Live Session","Sesión en vivo",
  "Música en vivo","Quito","Ecuador","Estudio","Grabación en vivo","Música Latinoamericana"
];

const TEMPLATE_DESC = `TDF Sessions — Sesión en vivo
Artista: {{ARTISTA}}
Grabado en: TDF (Quito, Ecuador)
Fecha: {{FECHA}}

Créditos:
• Producción: TDF RECORDS
• Ingeniería de audio:
• Edición y post:

Escucha más: https://youtube.com/playlist?list=PLORPSiW9rnkjSYKaBSAX-QqoVf_9b29EP
#TDFSessions #SesiónEnVivo #MúsicaEnVivo #Quito #Ecuador`;

export function ScheduleModal({open,onClose,projectId,onOk,defaults,api}:Props){
  const [meta, setMeta] = useState<Meta>({
    ytTitle: defaults?.ytTitle ?? "",
    ytDescription: defaults?.ytDescription ?? TEMPLATE_DESC,
    ytTags: defaults?.ytTags ?? DEFAULT_TAGS,
    ytPlaylistId: defaults?.ytPlaylistId ?? "PLORPSiW9rnkjSYKaBSAX-QqoVf_9b29EP",
    ytPublishAt: defaults?.ytPublishAt ?? dayjs().add(3,"day").hour(19).minute(0).second(0).format("YYYY-MM-DDTHH:mm:ss-05:00"),
    ytVideoFileId: defaults?.ytVideoFileId ?? "",
    ytThumbFileId: defaults?.ytThumbFileId ?? ""
  });
  const [submitting,setSubmitting] = useState(false);
  const onSubmit = async () => {
    setSubmitting(true);
    try {
      await api.post(`/projects/${projectId}/stage`, {
        toStage: "Schedule",
        payload: {
          project_id: projectId,
          title: meta.ytTitle,
          description: meta.ytDescription,
          tags: meta.ytTags.join(","),
          playlist_id: meta.ytPlaylistId,
          publish_at: meta.ytPublishAt,
          video_file_id: meta.ytVideoFileId,
          thumb_file_id: meta.ytThumbFileId,
          context: `liveSession: ${meta.ytTitle}`
        }
      });
      onOk();
    } catch (e:any) {
      alert(e?.response?.data?.missing ? `Faltan requisitos: ${e.response.data.missing.join(", ")}` : "Error al programar");
    } finally {
      setSubmitting(false);
    }
  };

  return (
    <Dialog open={open} onClose={onClose} maxWidth="md" fullWidth>
      <DialogTitle>Programar en YouTube</DialogTitle>
      <DialogContent dividers>
        <Stack spacing={2}>
          <TextField label="Título" value={meta.ytTitle} onChange={e=>setMeta({...meta, ytTitle:e.target.value})} required fullWidth />
          <TextField label="Descripción" value={meta.ytDescription} onChange={e=>setMeta({...meta, ytDescription:e.target.value})} multiline minRows={6} fullWidth />
          <TextField label="Tags (coma separada)" value={meta.ytTags.join(",")} onChange={e=>setMeta({...meta, ytTags:e.target.value.split(",").map(s=>s.trim()).filter(Boolean)})} fullWidth />
          <Stack direction="row" spacing={1}>{meta.ytTags.map(t=><Chip key={t} label={t} />)}</Stack>
          <TextField label="Playlist ID" value={meta.ytPlaylistId} onChange={e=>setMeta({...meta, ytPlaylistId:e.target.value})} fullWidth />
          <TextField label="Fecha/Hora de publicación (ISO con -05:00)" value={meta.ytPublishAt} onChange={e=>setMeta({...meta, ytPublishAt:e.target.value})} fullWidth />
          <TextField label="Google Drive fileId (video)" value={meta.ytVideoFileId} onChange={e=>setMeta({...meta, ytVideoFileId:e.target.value})} required fullWidth />
          <TextField label="Google Drive fileId (miniatura, opcional)" value={meta.ytThumbFileId} onChange={e=>setMeta({...meta, ytThumbFileId:e.target.value})} fullWidth />
        </Stack>
      </DialogContent>
      <DialogActions>
        <Button onClick={onClose} disabled={submitting}>Cancelar</Button>
        <Button onClick={onSubmit} variant="contained" disabled={submitting}>Programar</Button>
      </DialogActions>
    </Dialog>
  );
}
