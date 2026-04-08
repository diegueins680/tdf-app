import { useEffect, useMemo, useState } from 'react'
import {
  Button,
  Chip,
  Dialog,
  DialogActions,
  DialogContent,
  DialogTitle,
  Link,
  Stack,
  Typography,
} from '@mui/material'
import { fetchVersion, VersionInfo } from '../api/meta'

type Props = {
  open: boolean
  onClose: () => void
}

export default function AboutDialog({ open, onClose }: Props) {
  const [versionInfo, setVersionInfo] = useState<VersionInfo | null>(null)
  const apiBase = useMemo(() => import.meta.env.VITE_API_BASE ?? 'http://localhost:8080', [])
  const timezone = useMemo(() => import.meta.env.VITE_TZ ?? 'UTC', [])

  useEffect(() => {
    if (!open) return
    let cancelled = false
    fetchVersion(apiBase)
      .then((data) => {
        if (!cancelled) setVersionInfo(data)
      })
      .catch(() => {
        if (!cancelled) setVersionInfo(null)
      })
    return () => {
      cancelled = true
    }
  }, [open, apiBase])

  return (
    <Dialog open={open} onClose={onClose} maxWidth="sm" fullWidth>
      <DialogTitle>TDF HQ UI · Acerca de</DialogTitle>
      <DialogContent>
        <Stack spacing={1.5}>
          <Typography variant="body2">
            API Base:{' '}
            <Link href={apiBase} target="_blank" rel="noreferrer">
              {apiBase}
            </Link>
          </Typography>
          <Typography variant="body2">
            Zona horaria: <Chip label={timezone} size="small" />
          </Typography>
          <Typography variant="body2">
            Versión del API:{' '}
            {versionInfo ? (
              <Stack spacing={0.5}>
                <Stack direction="row" spacing={1} alignItems="center">
                  <Chip label={versionInfo.version} size="small" />
                  {versionInfo.name ? (
                    <Chip label={versionInfo.name} size="small" variant="outlined" />
                  ) : null}
                  {versionInfo.commit ? (
                    <Link
                      href={`https://github.com/diegueins680/TDF/commit/${versionInfo.commit}`}
                      target="_blank"
                      rel="noreferrer"
                      sx={{ fontSize: 12 }}
                    >
                      {versionInfo.commit.slice(0, 7)}
                    </Link>
                  ) : null}
                </Stack>
                {versionInfo.buildTime ? (
                  <Typography variant="caption" color="text.secondary">
                    Built: {new Date(versionInfo.buildTime).toUTCString()}
                  </Typography>
                ) : null}
              </Stack>
            ) : (
              'n/d'
            )}
          </Typography>
        </Stack>
      </DialogContent>
      <DialogActions>
        <Button onClick={onClose}>Cerrar</Button>
      </DialogActions>
    </Dialog>
  )
}
