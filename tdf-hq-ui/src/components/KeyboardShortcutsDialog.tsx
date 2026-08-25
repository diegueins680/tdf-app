import { Dialog, DialogTitle, DialogContent, List, ListItem, ListItemText, Box } from '@mui/material';

interface Props {
  open: boolean;
  onClose: () => void;
}

const shortcuts = [
  { keys: ['⌘', 'K'], description: 'Buscar y navegar' },
  { keys: ['/'], description: 'Enfocar búsqueda lateral' },
  { keys: ['Esc'], description: 'Cerrar menú lateral (móvil)' },
  { keys: ['?'], description: 'Mostrar atajos de teclado' },
];

export function KeyboardShortcutsDialog({ open, onClose }: Props) {
  return (
    <Dialog open={open} onClose={onClose} maxWidth="xs" fullWidth aria-labelledby="keyboard-shortcuts-dialog-title">
      <DialogTitle id="keyboard-shortcuts-dialog-title">Atajos de teclado</DialogTitle>
      <DialogContent>
        <List dense>
          {shortcuts.map(({ keys, description }) => (
            <ListItem key={description} disableGutters>
              <Box sx={{ display: 'flex', gap: 0.5, mr: 2, minWidth: 80 }}>
                {keys.map(k => (
                  <Box key={k} component="kbd" sx={{ px: 1, py: 0.25, bgcolor: 'action.hover', borderRadius: 1, fontSize: '0.75rem', fontFamily: 'monospace' }}>
                    {k}
                  </Box>
                ))}
              </Box>
              <ListItemText primary={description} primaryTypographyProps={{ variant: 'body2' }} />
            </ListItem>
          ))}
        </List>
      </DialogContent>
    </Dialog>
  );
}
