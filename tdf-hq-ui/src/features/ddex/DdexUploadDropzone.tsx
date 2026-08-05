import React, { useCallback, useState } from 'react';
import {
  Typography,
  Paper,
  Alert,
  LinearProgress,
  Stack,
} from '@mui/material';
import { CloudUpload as UploadIcon } from '@mui/icons-material';
import { useDropzone } from 'react-dropzone';
import { useMutation } from '@tanstack/react-query';
import { DDEX } from '../../api/ddex';

interface DdexUploadDropzoneProps {
  onUploadComplete: () => void;
}

export const DdexUploadDropzone: React.FC<DdexUploadDropzoneProps> = ({
  onUploadComplete,
}) => {
  const [uploadError, setUploadError] = useState<string | null>(null);

  const uploadMutation = useMutation({
    mutationFn: async (file: File) => {
      // Read file as base64
      const base64 = await new Promise<string>((resolve, reject) => {
        const reader = new FileReader();
        reader.onload = () => {
          const dataUrl = reader.result as string;
          // Remove data URL prefix
          const base64Data = dataUrl.split(',')[1];
          resolve(base64Data);
        };
        reader.onerror = reject;
        reader.readAsDataURL(file);
      });

      return DDEX.uploadDocument({
        uploadFileName: file.name,
        uploadContentType: file.type || 'application/xml',
        uploadContentBase64: base64,
      });
    },
    onSuccess: () => {
      setUploadError(null);
      onUploadComplete();
    },
    onError: (error) => {
      setUploadError(error.message);
    },
  });

  const onDrop = useCallback((acceptedFiles: File[]) => {
    if (acceptedFiles.length > 0) {
      const file = acceptedFiles[0];
      // Validate file type
      const validTypes = ['application/xml', 'text/xml', ''];
      const isValidType = validTypes.includes(file.type) || file.name.endsWith('.xml');
      
      if (!isValidType) {
        setUploadError('Please upload an XML file');
        return;
      }

      uploadMutation.mutate(file);
    }
  }, [uploadMutation]);

  const { getRootProps, getInputProps, isDragActive } = useDropzone({
    onDrop,
    accept: {
      'application/xml': ['.xml'],
      'text/xml': ['.xml'],
    },
    maxFiles: 1,
    disabled: uploadMutation.isPending,
  });

  return (
    <Paper
      {...getRootProps()}
      sx={{
        p: 4,
        textAlign: 'center',
        cursor: uploadMutation.isPending ? 'wait' : 'pointer',
        border: '2px dashed',
        borderColor: isDragActive ? 'primary.main' : 'divider',
        bgcolor: isDragActive ? 'action.hover' : 'background.paper',
        transition: 'all 0.2s ease',
        '&:hover': {
          borderColor: 'primary.main',
          bgcolor: 'action.hover',
        },
      }}
    >
      <input {...getInputProps()} />
      <Stack spacing={2} alignItems="center">
        <UploadIcon sx={{ fontSize: 48, color: 'primary.main' }} />
        
        {uploadMutation.isPending ? (
          <>
            <Typography>Uploading {uploadMutation.variables?.name}...</Typography>
            <LinearProgress sx={{ width: '100%', maxWidth: 400 }} />
          </>
        ) : isDragActive ? (
          <Typography>Drop the XML file here...</Typography>
        ) : (
          <>
            <Typography variant="h6">
              Drag & drop a DDEX XML file here
            </Typography>
            <Typography variant="body2" color="text.secondary">
              or click to select a file
            </Typography>
            <Typography variant="caption" color="text.secondary">
              Supports ERN 4.3.2, RIN 2.1, and other DDEX formats
            </Typography>
          </>
        )}

        {uploadError && (
          <Alert severity="error" sx={{ mt: 2, width: '100%' }}>
            {uploadError}
          </Alert>
        )}

        {uploadMutation.isSuccess && (
          <Alert severity="success" sx={{ mt: 2, width: '100%' }}>
            Document uploaded successfully!
          </Alert>
        )}
      </Stack>
    </Paper>
  );
};
