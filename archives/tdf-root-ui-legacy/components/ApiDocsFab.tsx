import { Fab, Tooltip } from '@mui/material';
import HelpOutlineIcon from '@mui/icons-material/HelpOutline';
import * as React from 'react';

export default function ApiDocsFab() {
  const base = (import.meta.env.VITE_API_BASE as string | undefined) || '';
  const openDocs = React.useCallback(() => {
    const url = `${base.replace(/\/$/, '')}/docs`;
    window.open(url, '_blank', 'noopener,noreferrer');
  }, [base]);

  return (
    <Tooltip title="Open API Docs">
      <Fab
        onClick={openDocs}
        aria-label="API Docs"
        sx={{
          position: 'fixed',
          bottom: 88, // keeps distance from any bottom-right status widget
          left: 16,
          zIndex: (t) => t.zIndex.tooltip,
        }}
      >
        <HelpOutlineIcon />
      </Fab>
    </Tooltip>
  );
}

