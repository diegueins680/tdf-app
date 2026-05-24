import { useState, useEffect, useRef } from 'react';
import { useQuery } from '@tanstack/react-query';
import {
  Dialog,
  DialogTitle,
  DialogContent,
  DialogActions,
  Button,
  Typography,
  Box,
  CircularProgress,
  Alert,
  Paper,
  Divider,
} from '@mui/material';
import QRCode from 'qrcode';
import DownloadIcon from '@mui/icons-material/Download';
import PrintIcon from '@mui/icons-material/Print';
import { SocialEventsAPI, type TicketDTO } from '../api/socialEvents';

interface TicketQRDisplayProps {
  open: boolean;
  onClose: () => void;
  eventId: string;
  eventTitle: string;
  ticket: TicketDTO;
}

export function TicketQRDisplay({ open, onClose, eventId, eventTitle, ticket }: TicketQRDisplayProps) {
  const canvasRef = useRef<HTMLCanvasElement>(null);
  const [qrGenerated, setQrGenerated] = useState(false);

  const ticketQRQuery = useQuery({
    queryKey: ['ticket-qr', eventId, ticket.ticketId],
    queryFn: () => SocialEventsAPI.getTicketQR(eventId, ticket.ticketId),
    enabled: open,
  });

  useEffect(() => {
    if (!ticketQRQuery.data || !canvasRef.current) return;

    const generateQR = async () => {
      try {
        await QRCode.toCanvas(canvasRef.current!, ticketQRQuery.data.twqQRData, {
          width: 300,
          margin: 2,
          errorCorrectionLevel: 'H',
        });
        setQrGenerated(true);
      } catch (err) {
        console.error('Failed to generate QR code:', err);
      }
    };

    generateQR();
  }, [ticketQRQuery.data]);

  const handleDownload = () => {
    if (!canvasRef.current) return;

    const link = document.createElement('a');
    link.download = `ticket-${ticket.ticketCode}.png`;
    link.href = canvasRef.current.toDataURL();
    link.click();
  };

  const handlePrint = () => {
    window.print();
  };

  return (
    <Dialog open={open} onClose={onClose} maxWidth="sm" fullWidth>
      <DialogTitle>
        Ticket QR Code
        <Typography variant="body2" color="text.secondary">
          {eventTitle}
        </Typography>
      </DialogTitle>

      <DialogContent>
        {ticketQRQuery.isLoading && (
          <Box sx={{ display: 'flex', justifyContent: 'center', p: 4 }}>
            <CircularProgress />
          </Box>
        )}

        {ticketQRQuery.isError && (
          <Alert severity="error">
            Failed to load QR code. Please try again.
          </Alert>
        )}

        {ticketQRQuery.data && (
          <Box>
            <Paper
              elevation={3}
              sx={{
                p: 3,
                display: 'flex',
                flexDirection: 'column',
                alignItems: 'center',
                '@media print': {
                  boxShadow: 'none',
                  border: '1px solid #000',
                },
              }}
            >
              <Typography variant="h6" gutterBottom>
                {ticket.ticketHolderName}
              </Typography>
              <Typography variant="body2" color="text.secondary" gutterBottom>
                {ticket.ticketHolderEmail}
              </Typography>

              <Divider sx={{ width: '100%', my: 2 }} />

              <Box
                sx={{
                  display: 'flex',
                  justifyContent: 'center',
                  alignItems: 'center',
                  mb: 2,
                }}
              >
                <canvas ref={canvasRef} />
              </Box>

              <Typography variant="h6" fontFamily="monospace" gutterBottom>
                {ticket.ticketCode}
              </Typography>

              <Divider sx={{ width: '100%', my: 2 }} />

              <Box sx={{ textAlign: 'center', width: '100%' }}>
                <Typography variant="body2" color="text.secondary">
                  Tier: {ticketQRQuery.data.twqTierName || 'General'}
                </Typography>
                {ticketQRQuery.data.twqPrice && (
                  <Typography variant="body2" color="text.secondary">
                    Price: {ticketQRQuery.data.twqCurrency} {(ticketQRQuery.data.twqPrice / 100).toFixed(2)}
                  </Typography>
                )}
              </Box>
            </Paper>

            <Alert severity="info" sx={{ mt: 2, '@media print': { display: 'none' } }}>
              <Typography variant="body2">
                Present this QR code at the event entrance for check-in. You can download or print this ticket.
              </Typography>
            </Alert>
          </Box>
        )}
      </DialogContent>

      <DialogActions sx={{ '@media print': { display: 'none' } }}>
        <Button onClick={onClose}>Close</Button>
        <Button
          onClick={handleDownload}
          startIcon={<DownloadIcon />}
          disabled={!qrGenerated}
        >
          Download
        </Button>
        <Button
          onClick={handlePrint}
          startIcon={<PrintIcon />}
          variant="contained"
          disabled={!qrGenerated}
        >
          Print
        </Button>
      </DialogActions>
    </Dialog>
  );
}
