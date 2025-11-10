import { useState } from 'react';
import { Avatar, Divider, IconButton, Menu, MenuItem, Stack, Tooltip, Typography } from '@mui/material';
import LogoutIcon from '@mui/icons-material/Logout';
import type { MouseEvent } from 'react';
import { useNavigate } from 'react-router-dom';
import { useSession } from '../session/SessionContext';

export default function SessionMenu() {
  const { session, logout } = useSession();
  const navigate = useNavigate();
  const [anchorEl, setAnchorEl] = useState<HTMLElement | null>(null);

  if (!session) {
    return null;
  }

  const handleOpen = (event: MouseEvent<HTMLElement>) => {
    setAnchorEl(event.currentTarget);
  };

  const handleClose = () => {
    setAnchorEl(null);
  };

  const handleLogout = () => {
    logout();
    setAnchorEl(null);
    navigate('/login', { replace: true });
  };

  const initials = session.displayName?.charAt(0).toUpperCase() || session.username.charAt(0).toUpperCase();
  const rolesLabel = session.roles.length ? session.roles.join(', ') : 'Sin roles asignados';

  return (
    <>
      <Tooltip title={session.displayName} placement="bottom">
        <IconButton color="inherit" onClick={handleOpen} size="small" aria-label="Abrir menú de sesión">
          <Avatar sx={{ width: 32, height: 32, fontSize: 16 }}>{initials}</Avatar>
        </IconButton>
      </Tooltip>
      <Menu
        anchorEl={anchorEl}
        open={Boolean(anchorEl)}
        onClose={handleClose}
        anchorOrigin={{ vertical: 'bottom', horizontal: 'right' }}
        transformOrigin={{ vertical: 'top', horizontal: 'right' }}
      >
        <MenuItem disabled>
          <Stack spacing={0.5}>
            <Typography variant="subtitle2">{session.displayName}</Typography>
            <Typography variant="body2" color="text.secondary">{rolesLabel}</Typography>
          </Stack>
        </MenuItem>
        <Divider sx={{ my: 0.5 }} />
        <MenuItem onClick={handleLogout}>
          <Stack direction="row" spacing={1} alignItems="center">
            <LogoutIcon fontSize="small" />
            <Typography variant="body2">Cerrar sesión</Typography>
          </Stack>
        </MenuItem>
      </Menu>
    </>
  );
}
