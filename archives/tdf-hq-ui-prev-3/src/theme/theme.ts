import { createTheme } from '@mui/material/styles';

const theme = createTheme({
  palette: {
    primary: { main: '#111827' }, // near-black
    secondary: { main: '#00bcd4' }, // teal accent
  },
  shape: { borderRadius: 10 },
});

export default theme;
