import { useRef, useState } from 'react';
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import {
  Badge, IconButton, Popover, List, ListItemButton, ListItemText, Typography,
  Stack, Button, Divider, Box, CircularProgress,
} from '@mui/material';
import { Notifications as NotificationsIcon, DoneAll as DoneAllIcon } from '@mui/icons-material';
import { useTranslation } from 'react-i18next';
import { Fans } from '../api/fans';
import type { NotificationDTO } from '../api/types';
import LazyPaginatedList from './LazyPaginatedList';
import { NOTIFICATION_BELL_CONTRACTS } from './NotificationBell.contracts';

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
  const { t } = useTranslation();
  const [anchorEl, setAnchorEl] = useState(null as HTMLElement | null);
  const triggerRef = useRef(null as HTMLButtonElement | null);
  const headingRef = useRef(null as HTMLHeadingElement | null);

  const countQuery = useQuery({
    queryKey: ['notification-count'],
    queryFn: () => Fans.getNotificationCount(),
    refetchInterval: NOTIFICATION_BELL_CONTRACTS.countRefetchIntervalMs,
  });

  const listQuery = useQuery({
    queryKey: ['notifications'],
    queryFn: () => Fans.listNotifications(),
    enabled: Boolean(anchorEl),
  });

  const markReadMut = useMutation({
    mutationFn: (notifId: number) => Fans.markNotificationRead(notifId),
    onSuccess: () => {
      void qc.invalidateQueries({ queryKey: ['notifications'] });
      void qc.invalidateQueries({ queryKey: ['notification-count'] });
    },
  });

  const markAllMut = useMutation({
    mutationFn: () => Fans.markAllNotificationsRead(),
    onSuccess: () => {
      void qc.invalidateQueries({ queryKey: ['notifications'] });
      void qc.invalidateQueries({ queryKey: ['notification-count'] });
    },
  });

  const unreadCount = countQuery.data?.ncUnread ?? 0;
  const notifications: NotificationDTO[] = listQuery.data ?? [];
  const isListLoading = listQuery.isLoading || (listQuery.isFetching && notifications.length === 0);
  const isEmpty = !isListLoading && notifications.length === 0;
  const pendingReadId = markReadMut.isPending ? markReadMut.variables : undefined;
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

  const focusAfterMarkRead = (notification: NotificationDTO, target: HTMLElement) => {
    if (notification.nIsRead || markReadMut.isPending) {
      focusSoon(() => target);
      return;
    }
    markReadMut.mutate(notification.nId, { onSettled: () => focusSoon(() => target) });
  };

  const focusAfterMarkReadKeyDown = (event: KeyboardTargetEvent, notification: NotificationDTO) => {
    if (!isActivationKey(event.key)) return;
    event.preventDefault();
    focusAfterMarkRead(notification, event.currentTarget);
  };

  const focus = {
    afterOpenClick: focusAfterOpenClick,
    afterOpenKeyDown: focusAfterOpenKeyDown,
    afterClose: focusAfterClose,
    afterMarkAll: focusAfterMarkAll,
    afterMarkAllKeyDown: focusAfterMarkAllKeyDown,
    afterMarkRead: focusAfterMarkRead,
    afterMarkReadKeyDown: focusAfterMarkReadKeyDown,
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
                    onClick={(event) => focus.afterMarkRead(n, event.currentTarget)}
                    onKeyDown={(event) => focus.afterMarkReadKeyDown(event, n)}
                    disabled={pendingReadId === n.nId}
                    aria-busy={pendingReadId === n.nId ? true : undefined}
                    sx={{ bgcolor: n.nIsRead ? 'transparent' : 'action.hover' }}
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
    </>
  );
}
