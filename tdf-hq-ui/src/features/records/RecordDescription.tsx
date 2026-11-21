import { Box, Button, Dialog, DialogActions, DialogContent, DialogTitle, Typography } from '@mui/material';
import { useId, useLayoutEffect, useRef, useState } from 'react';
import MarkdownText from '../../components/MarkdownText';

export default function RecordDescription({ text, title, plain = false }: { text: string; title: string; plain?: boolean }) {
  const preview = useRef<HTMLDivElement>(null);
  const content = useRef<HTMLDivElement>(null);
  const [clipped, setClipped] = useState(false);
  const [open, setOpen] = useState(false);
  const titleId = useId();

  useLayoutEffect(() => {
    const viewport = preview.current;
    const body = content.current;
    if (!viewport || !body) return;
    const measure = () => {
      const overflows = body.offsetHeight > viewport.clientHeight;
      body.toggleAttribute('inert', overflows);
      setClipped(overflows);
    };
    measure();
    if (typeof ResizeObserver === 'undefined') return;
    const observer = new ResizeObserver(measure);
    observer.observe(viewport);
    observer.observe(body);
    return () => observer.disconnect();
  }, [text]);

  if (!text) return null;
  const description = plain
    ? <Typography variant="body2" sx={{ overflowWrap: 'anywhere' }}>{text}</Typography>
    : <MarkdownText variant="body2">{text}</MarkdownText>;

  return (
    <Box sx={{ containerType: 'inline-size', color: 'text.secondary' }}>
      <Box ref={preview} sx={{ maxHeight: 'min(4.5rem, 40cqw)', overflow: 'hidden' }}>
        <div ref={content}>{description}</div>
      </Box>
      {clipped && <Button size="small" aria-haspopup="dialog" onClick={() => setOpen(true)}>Leer más</Button>}
      <Dialog open={open} onClose={() => setOpen(false)} aria-labelledby={titleId} fullWidth maxWidth="sm">
        <DialogTitle id={titleId}>{title}</DialogTitle>
        <DialogContent dividers>{description}</DialogContent>
        <DialogActions><Button onClick={() => setOpen(false)}>Cerrar</Button></DialogActions>
      </Dialog>
    </Box>
  );
}
