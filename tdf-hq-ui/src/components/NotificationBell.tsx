import { useState } from 'react';
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import {
  Badge, IconButton, Popover, List, ListItemButton, ListItemText, Typography,
  Stack, Button, Divider, Box,
} from '@mui/material';
import { Notifications as NotificationsIcon, DoneAll as DoneAllIcon } from '@mui/icons-material';
import { Fans } from '../api/fans';
import type { NotificationDTO } from '../api/types';

export default function NotificationBell() {
  const qc = useQueryClient();
  const [anchorEl, setAnchorEl] = useState<HTMLElement | null>(null);

  const countQuery = useQuery({
    queryKey: ['notification-count'],
    queryFn: () => Fans.getNotificationCount(),
    refetchInterval: 30000,
  });

  const listQuery = useQuery({
    queryKey: ['notifications'],
    queryFn: () => Fans.listNotifications(),
    enabled: Boolean(anchorEl),
  });

  const markReadMut = useMutation({
    mutationFn: (notifId: number) => Fans.markNotificationRead(notifId),
    onSuccess: () => {
      qc.invalidateQueries({ queryKey: ['notifications'] });
      qc.invalidateQueries({ queryKey: ['notification-count'] });
    },
  });

  const markAllMut = useMutation({
    mutationFn: () => Fans.markAllNotificationsRead(),
    onSuccess: () => {
      qc.invalidateQueries({ queryKey: ['notifications'] });
      qc.invalidateQueries({ queryKey: ['notification-count'] });
    },
  });

  const unreadCount = countQuery.data?.ncUnread ?? 0;
  const notifications: NotificationDTO[] = listQuery.data ?? [];

  return (
    <>
      <IconButton
        color="inherit"
        onClick={(e) => setAnchorEl(e.currentTarget)}
        aria-label="Notificaciones"
      >
        <Badge badgeContent={unreadCount} color="error" max={99}>
          <NotificationsIcon />
        </Badge>
      </IconButton>

      <Popover
        open={Boolean(anchorEl)}
        anchorEl={anchorEl}
        onClose={() => setAnchorEl(null)}
        anchorOrigin={{ vertical: 'bottom', horizontal: 'right' }}
        transformOrigin={{ vertical: 'top', horizontal: 'right' }}
        slotProps={{ paper: { sx: { width: 360, maxHeight: 480 } } }}
      >
        <Stack direction="row" alignItems="center" justifyContent="space-between" sx={{ px: 2, py: 1 }}>
          <Typography variant="subtitle1" fontWeight={700}>Notificaciones</Typography>
          {unreadCount > 0 && (
            <Button
              size="small"
              startIcon={<DoneAllIcon />}
              onClick={() => markAllMut.mutate()}
              disabled={markAllMut.isPending}
            >
              Leer todo
            </Button>
          )}
        </Stack>
        <Divider />
        {notifications.length === 0 ? (
          <Box sx={{ p: 3, textAlign: 'center' }}>
            <Typography variant="body2" color="text.secondary">Sin notificaciones</Typography>
          </Box>
        ) : (
          <List dense disablePadding sx={{ overflow: 'auto', maxHeight: 380 }}>
            {notifications.map((n) => (
              <ListItemButton
                key={n.nId}
                onClick={() => { if (!n.nIsRead) markReadMut.mutate(n.nId); }}
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
      </Popover>
    </>
  );
}
