import { useEffect, useRef, useState, type MouseEvent } from 'react';
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import {
  Badge, IconButton, Popover, List, ListItemButton, ListItemText, Typography,
  Stack, Button, Divider, Box, CircularProgress, Alert, Snackbar,
} from '@mui/material';
import NotificationsIcon from '@mui/icons-material/Notifications';
import DoneAllIcon from '@mui/icons-material/DoneAll';
import { useTranslation } from 'react-i18next';
import { Link as RouterLink, useNavigate } from 'react-router-dom';
import { Fans } from '../api/fans';
import type { NotificationDTO } from '../api/types';
import LazyPaginatedList from './LazyPaginatedList';
import { NOTIFICATION_BELL_CONTRACTS } from './NotificationBell.contracts';
import { notificationLink, notificationTargetPath } from './notificationTarget';
import { useSession } from '../session/SessionContext';

interface TargetEvent {
  currentTarget: HTMLElement;
}

interface KeyboardTargetEvent extends TargetEvent {
  key: string;
  preventDefault: () => void;
}

function isActivationKey(key: string): boolean {
  return key === 'Enter' || key === ' ';
}

function focusSoon(getTarget: () => HTMLElement | null): void {
  globalThis.setTimeout(() => getTarget()?.focus(), 0);
}

export default function NotificationBell() {
  const qc = useQueryClient();
  const navigate = useNavigate();
  const { session } = useSession();
  const accountId = session?.partyId;
  const [readError, setReadError] = useState(false);
  useEffect(() => {
    const showReadError = () => setReadError(true);
    window.addEventListener('tdf-notification-read-error', showReadError);
    return () => window.removeEventListener('tdf-notification-read-error', showReadError);
  }, []);
  const { t } = useTranslation();
  const [anchorEl, setAnchorEl] = useState(null as HTMLElement | null);
  const triggerRef = useRef(null as HTMLButtonElement | null);
  const headingRef = useRef(null as HTMLHeadingElement | null);

  const countQuery = useQuery({
    queryKey: ['notification-count', accountId],
    queryFn: () => Fans.getNotificationCount(),
    refetchInterval: NOTIFICATION_BELL_CONTRACTS.countRefetchIntervalMs,
  });

  const listQuery = useQuery({
    queryKey: ['notifications', accountId],
    queryFn: () => Fans.listNotifications(),
    enabled: Boolean(anchorEl),
  });

  const markReadMut = useMutation({
    mutationFn: (notifId: number) => Fans.markNotificationRead(notifId),
    onError: () => setReadError(true),
    onSuccess: () => {
      void qc.invalidateQueries({ queryKey: ['notifications'] });
      void qc.invalidateQueries({ queryKey: ['notification-count'] });
    },
  });

  const markAllMut = useMutation({
    mutationFn: () => Fans.markAllNotificationsRead(),
    onError: () => setReadError(true),
    onSuccess: () => {
      void qc.invalidateQueries({ queryKey: ['notifications'] });
      void qc.invalidateQueries({ queryKey: ['notification-count'] });
    },
  });

  const unreadCount = countQuery.data?.ncUnread ?? 0;
  const notifications: NotificationDTO[] = listQuery.data ?? [];
  const isListLoading = listQuery.isLoading || (listQuery.isFetching && notifications.length === 0);
  const isEmpty = !isListLoading && notifications.length === 0;
  const copy = {
    title: t('notifications.title'),
    markAll: t('notifications.markAll'),
    markAllLoading: t('notifications.markAllLoading'),
    loading: t('notifications.loading'),
    empty: t('notifications.empty'),
  };

  const focusPanelHeading = () => focusSoon(() => headingRef.current);
  const focusTrigger = () => focusSoon(() => triggerRef.current);

  const focusAfterOpenClick = (event: TargetEvent) => {
    setAnchorEl(event.currentTarget);
    focusPanelHeading();
  };

  const focusAfterOpenKeyDown = (event: KeyboardTargetEvent) => {
    if (!isActivationKey(event.key)) return;
    event.preventDefault();
    setAnchorEl(event.currentTarget);
    focusPanelHeading();
  };

  const focusAfterClose = () => {
    setAnchorEl(null);
    focusTrigger();
  };

  const focusAfterMarkAll = () => {
    if (markAllMut.isPending) return;
    markAllMut.mutate(undefined, { onSettled: focusPanelHeading });
  };

  const focusAfterMarkAllKeyDown = (event: KeyboardTargetEvent) => {
    if (!isActivationKey(event.key)) return;
    event.preventDefault();
    focusAfterMarkAll();
  };

  const activate = (notification: NotificationDTO, event: MouseEvent<HTMLElement>) => {
    if (event.button !== 0 && event.button !== 1) return;
    if (event.button === 0 && !event.ctrlKey && !event.metaKey && !event.shiftKey && !event.altKey) {
      event.preventDefault();
      if (!notification.nIsRead) markReadMut.mutate(notification.nId);
      setAnchorEl(null);
      navigate(notificationTargetPath(notification) ?? notificationLink(notification), { state: { activatedNotificationId: notification.nId } });
    }
  };

  const focus = {
    afterOpenClick: focusAfterOpenClick,
    afterOpenKeyDown: focusAfterOpenKeyDown,
    afterClose: focusAfterClose,
    afterMarkAll: focusAfterMarkAll,
    afterMarkAllKeyDown: focusAfterMarkAllKeyDown,
  };

  return (
    <>
      <IconButton
        ref={triggerRef}
        color="inherit"
        onClick={focus.afterOpenClick}
        onKeyDown={focus.afterOpenKeyDown}
        aria-label={copy.title}
        aria-busy={countQuery.isLoading ? true : undefined}
        sx={{ minWidth: 44, minHeight: 44 }}
      >
        <Badge badgeContent={unreadCount} color="error" max={NOTIFICATION_BELL_CONTRACTS.badgeMaxDisplayCount}>
          {countQuery.isLoading ? (
            <CircularProgress
              size={NOTIFICATION_BELL_CONTRACTS.triggerLoadingSpinnerSizePx}
              color="inherit"
              aria-label={copy.loading}
            />
          ) : (
            <NotificationsIcon />
          )}
        </Badge>
      </IconButton>

      <Popover
        open={Boolean(anchorEl)}
        anchorEl={anchorEl}
        onClose={focus.afterClose}
        anchorOrigin={{ vertical: 'bottom', horizontal: 'right' }}
        transformOrigin={{ vertical: 'top', horizontal: 'right' }}
        slotProps={{
          paper: {
            sx: {
              width: NOTIFICATION_BELL_CONTRACTS.popoverPaperWidthPx,
              maxHeight: NOTIFICATION_BELL_CONTRACTS.popoverPaperMaxHeightPx,
            },
          },
        }}
      >
        <Stack direction="row" alignItems="center" justifyContent="space-between" sx={{ px: 2, py: 1 }}>
          <Typography
            ref={headingRef}
            tabIndex={-1}
            variant="subtitle1"
            component="h2"
            fontWeight={NOTIFICATION_BELL_CONTRACTS.headingFontWeight}
          >
            {copy.title}
          </Typography>
          {unreadCount > 0 && (
            <Button
              size="small"
              disabled={markAllMut.isPending}
              onClick={focus.afterMarkAll}
              onKeyDown={focus.afterMarkAllKeyDown}
              aria-busy={markAllMut.isPending ? true : undefined}
              startIcon={
                markAllMut.isPending
                  ? (
                    <CircularProgress
                      size={NOTIFICATION_BELL_CONTRACTS.markAllActionSpinnerSizePx}
                      color="inherit"
                    />
                  )
                  : <DoneAllIcon />
              }
            >
              {markAllMut.isPending ? copy.markAllLoading : copy.markAll}
            </Button>
          )}
        </Stack>
        <Divider />
        {isListLoading ? (
          <Box
            sx={{ p: 3, textAlign: 'center' }}
            role="status"
            aria-live="polite"
          >
            <Stack spacing={1} alignItems="center">
              <CircularProgress size={NOTIFICATION_BELL_CONTRACTS.panelLoadingSpinnerSizePx} />
              <Typography variant="body2" color="text.secondary">
                {copy.loading}
              </Typography>
            </Stack>
          </Box>
        ) : isEmpty ? (
          <Box sx={{ p: 3, textAlign: 'center' }}>
            <Typography variant="body2" color="text.secondary">{copy.empty}</Typography>
          </Box>
        ) : (
          <LazyPaginatedList
            items={notifications}
            loading={listQuery.isFetching}
            pagination={{ itemLabel: copy.title.toLocaleLowerCase(), initialRowsPerPage: 10 }}
            renderItems={(visibleNotifications) => (
              <List dense disablePadding sx={{ maxHeight: NOTIFICATION_BELL_CONTRACTS.notificationListMaxHeightPx }}>
                {visibleNotifications.map((n) => (
                  <ListItemButton
                    key={n.nId}
                    component={RouterLink}
                    to={notificationLink(n)}
                    onClick={(event) => activate(n, event)}
                    onAuxClick={(event) => activate(n, event)}
                    sx={{ bgcolor: n.nIsRead ? 'transparent' : 'action.hover', minHeight: 44,
                      '&:focus-visible': { outline: '3px solid', outlineColor: 'primary.main', outlineOffset: -3 } }}
                  >
                    <ListItemText
                      primary={n.nTitle}
                      secondary={
                        <Stack component="span" spacing={0.5}>
                          <Typography variant="caption" component="span">{n.nBody}</Typography>
                          <Typography variant="caption" component="span" color="text.disabled">
                            {new Date(n.nCreatedAt).toLocaleString()}
                          </Typography>
                        </Stack>
                      }
                    />
                  </ListItemButton>
                ))}
              </List>
            )}
          />
        )}
      </Popover>
      <Snackbar open={readError} onClose={() => setReadError(false)}>
        <Alert severity="warning" onClose={() => setReadError(false)}>{t('notifications.readError')}</Alert>
      </Snackbar>
    </>
  );
}
