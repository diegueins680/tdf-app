import { createContext, useContext, useState, useCallback, type ReactNode } from 'react';
import { Snackbar, Alert, Button, type AlertColor } from '@mui/material';

interface Toast {
  id: number;
  message: string;
  severity: AlertColor;
  autoHideDuration?: number;
  action?: { label: string; onClick: () => void };
}

interface ToastContextValue {
  showToast: (message: string, severity?: AlertColor, autoHideDuration?: number) => void;
  showSuccess: (message: string) => void;
  showError: (message: string) => void;
  showInfo: (message: string) => void;
  showUndo: (message: string, onUndo: () => void, duration?: number) => void;
}

const ToastContext = createContext<ToastContextValue | null>(null);

let nextId = 0;

export function ToastProvider({ children }: { children: ReactNode }) {
  const [toasts, setToasts] = useState<Toast[]>([]);

  const handleClose = useCallback((id: number) => {
    setToasts(prev => prev.filter(t => t.id !== id));
  }, []);

  const showToast = useCallback((message: string, severity: AlertColor = 'info', autoHideDuration = 4000) => {
    const id = nextId++;
    setToasts(prev => [...prev, { id, message, severity, autoHideDuration }]);
  }, []);

  const showSuccess = useCallback((msg: string) => showToast(msg, 'success'), [showToast]);
  const showError = useCallback((msg: string) => showToast(msg, 'error', 6000), [showToast]);
  const showInfo = useCallback((msg: string) => showToast(msg, 'info'), [showToast]);

  const showUndo = useCallback((message: string, onUndo: () => void, duration = 5000) => {
    const id = nextId++;
    setToasts(prev => [...prev, {
      id,
      message,
      severity: 'info' as AlertColor,
      autoHideDuration: duration,
      action: { label: 'Deshacer', onClick: () => { onUndo(); handleClose(id); } },
    }]);
  }, [handleClose]);

  return (
    <ToastContext.Provider value={{ showToast, showSuccess, showError, showInfo, showUndo }}>
      {children}
      {toasts.map((toast, index) => (
        <Snackbar
          key={toast.id}
          open
          autoHideDuration={toast.action ? undefined : toast.autoHideDuration}
          onClose={() => handleClose(toast.id)}
          anchorOrigin={{ vertical: 'bottom', horizontal: 'center' }}
          sx={{ bottom: { xs: 16 + index * 56 } }}
        >
          <Alert
            onClose={() => handleClose(toast.id)}
            severity={toast.severity}
            variant="filled"
            sx={{ width: '100%' }}
            action={toast.action ? (
              <Button color="inherit" size="small" onClick={toast.action.onClick}>
                {toast.action.label}
              </Button>
            ) : undefined}
          >
            {toast.message}
          </Alert>
        </Snackbar>
      ))}
    </ToastContext.Provider>
  );
}

export function useToast() {
  const ctx = useContext(ToastContext);
  if (!ctx) throw new Error('useToast must be used inside ToastProvider');
  return ctx;
}
