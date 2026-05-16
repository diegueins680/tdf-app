
import { Alert } from '@mui/material';
export default function ErrorMessage({error}:{error: Error}) {
  return <Alert severity="error">{error.message}</Alert>;
}
