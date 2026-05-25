import MenuIcon from '@mui/icons-material/Menu';
import MenuOpenIcon from '@mui/icons-material/MenuOpen';
import SearchIcon from '@mui/icons-material/Search';
import { useEffect, useMemo, useState, useCallback, useRef } from 'react';
import {
  AppBar,
  Box,
  Button,
  IconButton,
  Stack,
  Toolbar,
  Typography,
  Dialog,
  DialogTitle,
  DialogContent,
  TextField,
  InputAdornment,
  List,
  ListItemButton,
  ListItemText,
  Tooltip,
} from '@mui/material';
import { Link as RouterLink, useLocation, useNavigate } from 'react-router-dom';
import SessionMenu from './SessionMenu';
import { useSession } from '../session/SessionContext';
import BrandLogo from './BrandLogo';
import NotificationBell from './NotificationBell';
import { NAV_GROUPS } from './SidebarNav';
import { canAccessPath } from '../utils/accessControl';
import { formatFriendlyPath } from '../utils/navigationLabels';

interface TopBarProps {
  onToggleSidebar?: () => void;
  sidebarOpen?: boolean;
}

export default function TopBar({ onToggleSidebar, sidebarOpen = true }: TopBarProps) {
  const { session } = useSession();
  const navigate = useNavigate();
  const location = useLocation();
  const [quickNavOpen, setQuickNavOpen] = useState(false);
  const [quickQuery, setQuickQuery] = useState('');
  const [quickHighlight, setQuickHighlight] = useState(0);
  const quickInputRef = useRef<HTMLInputElement | null>(null);

  const canUsePath = useCallback(
    (path: string) => canAccessPath(path, session?.roles, session?.modules),
    [session?.modules, session?.roles],
  );

  const quickNavItems = useMemo(() => {
    return NAV_GROUPS.flatMap((group) =>
      group.items
        .filter((item) => canUsePath(item.path))
        .map((item) => ({ ...item, group: group.title })),
    );
  }, [canUsePath]);

  const filteredQuickItems = useMemo(() => {
    const query = quickQuery.trim().toLowerCase();
    if (!query) return quickNavItems;
    return quickNavItems.filter(
      (item) =>
        item.label.toLowerCase().includes(query) || item.path.toLowerCase().includes(query),
    );
  }, [quickNavItems, quickQuery]);

  const openQuickNav = useCallback(() => {
    setQuickNavOpen(true);
    setQuickQuery('');
    setQuickHighlight(0);
  }, []);

  const closeQuickNav = () => {
    setQuickNavOpen(false);
    setQuickQuery('');
  };

  useEffect(() => {
    const handler = (event: KeyboardEvent) => {
      const tag = (event.target as HTMLElement | null)?.tagName?.toLowerCase();
      if (tag === 'input' || tag === 'textarea' || (event.target as HTMLElement | null)?.isContentEditable) return;
      if ((event.metaKey || event.ctrlKey) && event.key.toLowerCase() === 'k') {
        event.preventDefault();
        openQuickNav();
      }
    };
    window.addEventListener('keydown', handler);
    return () => window.removeEventListener('keydown', handler);
  }, [openQuickNav]);

  useEffect(() => {
    setQuickHighlight(0);
  }, [quickQuery]);

  useEffect(() => {
    if (quickNavOpen) {
      setTimeout(() => quickInputRef.current?.focus(), 0);
    }
  }, [quickNavOpen]);

  const handleSelectQuick = (idx: number) => {
    const target = filteredQuickItems[idx];
    if (!target) return;
    navigate(target.path);
    closeQuickNav();
  };

  return (
    <AppBar
      elevation={0}
      position="sticky"
      sx={{
        bgcolor: 'background.paper',
        borderBottom: '1px solid',
        borderColor: 'divider',
      }}
    >
      <Toolbar
        sx={{
          minHeight: { xs: 64, md: 72 },
          px: { xs: 2, md: 3 },
        }}
      >
        <Tooltip title={sidebarOpen ? 'Ocultar menú' : 'Mostrar menú'}>
          <IconButton
            edge="start"
            onClick={onToggleSidebar}
            sx={{ color: 'text.primary', mr: 1.5 }}
            aria-label={sidebarOpen ? 'Ocultar menú lateral' : 'Mostrar menú lateral'}
          >
            {sidebarOpen ? <MenuOpenIcon /> : <MenuIcon />}
          </IconButton>
        </Tooltip>

        <Box
          component={RouterLink}
          to="/inicio"
          sx={{
            display: { xs: 'none', sm: 'inline-flex' },
            alignItems: 'center',
            mr: 3,
          }}
          aria-label="Ir al inicio"
        >
          <BrandLogo
            variant="wordmark"
            size={42}
            sx={{
              height: { xs: 28, sm: 36, md: 42 },
              filter: (theme) =>
                theme.palette.mode === 'dark'
                  ? 'brightness(0) invert(1)'
                  : 'none',
            }}
          />
        </Box>

        <Stack direction="row" spacing={1} alignItems="center" sx={{ flexGrow: 1 }}>
          <Tooltip title="Cmd/Ctrl + K">
            <Button
              color="inherit"
              variant="outlined"
              onClick={openQuickNav}
              startIcon={<SearchIcon fontSize="small" />}
              sx={{
                textTransform: 'none',
                borderColor: 'divider',
                color: 'text.secondary',
                justifyContent: 'flex-start',
                minWidth: { xs: 40, sm: 220, md: 280 },
                px: { xs: 1.25, sm: 1.75 },
                py: 0.75,
              }}
              aria-keyshortcuts="Control+K Meta+K"
              aria-label="Buscar sección"
            >
              <Box component="span" sx={{ display: { xs: 'none', sm: 'inline' } }}>
                Buscar sección
              </Box>
              <Box
                component="span"
                sx={{
                  display: { xs: 'none', md: 'inline' },
                  ml: 'auto',
                  pl: 1.5,
                  fontSize: '0.75rem',
                  color: 'text.disabled',
                  fontWeight: 500,
                  letterSpacing: '0.04em',
                }}
              >
                ⌘K
              </Box>
            </Button>
          </Tooltip>
        </Stack>

        <Stack direction="row" spacing={1} alignItems="center" sx={{ ml: 'auto' }}>
          {session ? (
            <>
              <NotificationBell />
              <SessionMenu />
            </>
          ) : (
            <Button color="inherit" component={RouterLink} to="/login" sx={{ fontWeight: 600 }}>
              Ingresar
            </Button>
          )}
        </Stack>
      </Toolbar>

      {/* Quick-nav dialog */}
      <Dialog open={quickNavOpen} onClose={closeQuickNav} fullWidth maxWidth="sm">
        <DialogTitle sx={{ pb: 1 }}>Ir a otra sección</DialogTitle>
        <DialogContent>
          <TextField
            fullWidth
            label="Buscar secciones"
            placeholder="Escribe para buscar (ej: inventario, leads, marketplace)"
            value={quickQuery}
            onChange={(e) => setQuickQuery(e.target.value)}
            inputRef={quickInputRef}
            onKeyDown={(event) => {
              if (event.key === 'ArrowDown') {
                event.preventDefault();
                setQuickHighlight((prev) => (prev + 1) % Math.max(filteredQuickItems.length, 1));
              } else if (event.key === 'ArrowUp') {
                event.preventDefault();
                setQuickHighlight((prev) => {
                  if (filteredQuickItems.length === 0) return 0;
                  return prev <= 0 ? filteredQuickItems.length - 1 : prev - 1;
                });
              } else if (event.key === 'Enter') {
                event.preventDefault();
                handleSelectQuick(quickHighlight);
              } else if (event.key === 'Escape') {
                closeQuickNav();
              }
            }}
            InputProps={{
              startAdornment: (
                <InputAdornment position="start">
                  <SearchIcon fontSize="small" />
                </InputAdornment>
              ),
            }}
            sx={{ mb: 1.25 }}
          />
          {filteredQuickItems.length === 0 ? (
            <Typography variant="body2" color="text.secondary">
              Sin coincidencias. Prueba con otra palabra clave.
            </Typography>
          ) : (
            <List dense>
              {filteredQuickItems.slice(0, 30).map((item, idx) => (
                <ListItemButton
                  key={`${item.path}-${idx}`}
                  selected={idx === quickHighlight}
                  onClick={() => handleSelectQuick(idx)}
                  sx={{ borderRadius: 1.5, mb: 0.25 }}
                >
                  <ListItemText
                    primary={item.label}
                    secondary={item.group}
                    primaryTypographyProps={{ fontWeight: idx === quickHighlight ? 700 : 500 }}
                  />
                </ListItemButton>
              ))}
            </List>
          )}
        </DialogContent>
      </Dialog>
    </AppBar>
  );
}
