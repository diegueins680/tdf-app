import { useState } from 'react';
import { Avatar, Divider, IconButton, Menu, MenuItem, Stack, Tooltip, Typography } from '@mui/material';
import LogoutIcon from '@mui/icons-material/Logout';
import type { MouseEvent } from 'react';
import { useNavigate } from 'react-router-dom';
import { useSession } from '../session/SessionContext';
import { useTranslation } from 'react-i18next';
import { useMobileTelemetry } from '../mobile/telemetry';
import { canAccessPath } from '../utils/accessControl';

export default function SessionMenu() {
  const { session, logout } = useSession();
  const { t } = useTranslation();
  const navigate = useNavigate();
  const [anchorEl, setAnchorEl] = useState<HTMLElement | null>(null);
  const trackMobile = useMobileTelemetry('settings');

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
  const roles = Array.isArray(session.roles) ? session.roles : [];
  const rolesLabel = roles.length ? roles.join(', ') : t('sessionMenu.noRoles');
  // Help, legal and account pages live here instead of in the sidebar (audit decision D17).
  // The two public pages (WhatsApp preferences, account deletion) are offered to every account.
  const helpLink = (to: string, label: string, publicPage = false) => (
    publicPage || canAccessPath(to, session.roles, session.modules, session.featureFlags) ? [{ to, label }] : []
  );
  const helpLinks = [
    ...helpLink('/manual', 'sessionMenu.manual'),
    ...helpLink('/docs', 'sessionMenu.docs'),
    ...helpLink('/acerca', 'sessionMenu.about'),
    ...helpLink('/seguridad', 'sessionMenu.security'),
    ...helpLink('/whatsapp/consentimiento', 'sessionMenu.whatsapp', true),
    ...helpLink('/cuenta/eliminar', 'sessionMenu.deleteAccount', true),
  ];

  return (
    <>
      <Tooltip title={session.displayName} placement="bottom">
        <IconButton color="inherit" onClick={handleOpen} size="small" aria-label={t('sessionMenu.open')} sx={{ minWidth: 44, minHeight: 44 }}>
          <Avatar sx={{ width: 32, height: 32, fontSize: 16 }}>{initials}</Avatar>
        </IconButton>
      </Tooltip>
      <Menu
        anchorEl={anchorEl}
        open={Boolean(anchorEl)}
        onClose={handleClose}
        anchorOrigin={{ vertical: 'bottom', horizontal: 'right' }}
        transformOrigin={{ vertical: 'top', horizontal: 'right' }}
        MenuListProps={{ 'aria-label': 'Menú de sesión' }}
      >
        <MenuItem disabled>
          <Stack spacing={0.5}>
            <Typography variant="subtitle2">{session.displayName}</Typography>
            <Typography variant="body2" color="text.secondary">{rolesLabel}</Typography>
          </Stack>
        </MenuItem>
        <Divider sx={{ my: 0.5 }} />
        <MenuItem onClick={() => { handleClose(); trackMobile('mobile_testing_interest_clicked'); navigate('/app?surface=settings'); }}>{t('app.menu')}</MenuItem>
        <Divider sx={{ my: 0.5 }} />
        <Typography component="li" role="presentation" variant="caption" color="text.secondary" sx={{ display: 'block', px: 2, py: 0.5 }}>
          {t('sessionMenu.help')}
        </Typography>
        {helpLinks.map((link) => (
          <MenuItem key={link.to} onClick={() => { handleClose(); navigate(link.to); }}>{t(link.label)}</MenuItem>
        ))}
        <Divider sx={{ my: 0.5 }} />
        <MenuItem onClick={handleLogout}>
          <Stack direction="row" spacing={1} alignItems="center">
            <LogoutIcon fontSize="small" />
            <Typography variant="body2">{t('sessionMenu.logout')}</Typography>
          </Stack>
        </MenuItem>
      </Menu>
    </>
  );
}
