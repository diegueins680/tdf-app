
import { CircularProgress, Box } from '@mui/material';
export default function Loading() {
  return (
    <Box sx={{ display: 'flex', justifyContent: 'center', p: 3 }}>
      <CircularProgress />
    </Box>
  );
}
