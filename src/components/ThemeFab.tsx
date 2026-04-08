import { Fab, Tooltip } from '@mui/material'
import DarkModeIcon from '@mui/icons-material/DarkMode'
import LightModeIcon from '@mui/icons-material/LightMode'
import { useColorMode } from '../theme/ColorModeProvider'

const FAB_SPACING = 72

export default function ThemeFab() {
  const { mode, toggle } = useColorMode()
  const nextMode = mode === 'dark' ? 'claro' : 'oscuro'

  return (
    <Tooltip title={`Cambiar a modo ${nextMode}`}>
      <Fab
        aria-label="Cambiar tema"
        color="primary"
        onClick={toggle}
        size="small"
        sx={{
          position: 'fixed',
          right: 16,
          bottom: FAB_SPACING,
          zIndex: 1600,
          boxShadow: (theme) => theme.shadows[6],
        }}
      >
        {mode === 'dark' ? <LightModeIcon /> : <DarkModeIcon />}
      </Fab>
    </Tooltip>
  )
}
