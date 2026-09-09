import { useMemo, useState } from 'react';
import {
  Alert,
  Button,
  Checkbox,
  FormControlLabel,
  Rating,
  Stack,
  TextField,
  Typography,
} from '@mui/material';
import type { MerchReviewSubmit } from '../../api/merchReputation';

type ReviewKind = 'store' | 'product';

const labels = {
  es: {
    overall: 'Evaluación general',
    issue: 'Hubo un problema que la tienda tuvo que resolver',
    comment: 'Comentario opcional',
    submit: 'Enviar evaluación',
    edit: 'Guardar cambios',
    required: 'Completa todas las categorías requeridas.',
    noComparison: 'Evalúa solo esta compra. No necesitas comparar tiendas.',
  },
  en: {
    overall: 'Overall rating',
    issue: 'There was a problem the store had to resolve',
    comment: 'Optional comment',
    submit: 'Submit review',
    edit: 'Save changes',
    required: 'Complete every required category.',
    noComparison: 'Rate only this purchase. You do not need to compare stores.',
  },
} as const;

const dimensionNames = {
  es: {
    description_accuracy: 'Producto conforme a la descripción',
    product_quality: 'Calidad del producto',
    preparation_dispatch: 'Velocidad de preparación y despacho',
    communication: 'Comunicación',
    packaging: 'Empaque',
    problem_resolution: 'Resolución de problemas',
  },
  en: {
    description_accuracy: 'Product matched the description',
    product_quality: 'Product quality',
    preparation_dispatch: 'Preparation and dispatch speed',
    communication: 'Communication',
    packaging: 'Packaging',
    problem_resolution: 'Problem resolution',
  },
} as const;
type DimensionCode = keyof typeof dimensionNames.es;

export interface MerchReviewFormProps {
  kind: ReviewKind;
  cancelled?: boolean;
  expectedRevision: number;
  locale?: 'es' | 'en';
  initialValue?: Partial<MerchReviewSubmit>;
  submitting?: boolean;
  error?: string | null;
  onSubmit: (review: MerchReviewSubmit) => void;
}

export default function MerchReviewForm({
  kind,
  cancelled = false,
  expectedRevision,
  locale = 'es',
  initialValue,
  submitting = false,
  error,
  onSubmit,
}: MerchReviewFormProps) {
  const copy = labels[locale];
  const [overall, setOverall] = useState(initialValue?.overallRating ?? 0);
  const [issue, setIssue] = useState(initialValue?.issueOccurred ?? false);
  const [comment, setComment] = useState(initialValue?.comment ?? '');
  const [dimensions, setDimensions] = useState<Record<string, number>>(
    initialValue?.dimensions ?? {},
  );
  const [validationError, setValidationError] = useState<string | null>(null);

  const requiredDimensions = useMemo<DimensionCode[]>(() => {
    if (kind === 'product') return ['description_accuracy', 'product_quality'];
    const base = cancelled
      ? (['communication'] satisfies DimensionCode[])
      : (['preparation_dispatch', 'communication', 'packaging'] satisfies DimensionCode[]);
    return issue ? [...base, 'problem_resolution'] : [...base];
  }, [cancelled, issue, kind]);
  const shownError = validationError ?? error;

  const submit = () => {
    const missing = overall < 1 || requiredDimensions.some((dimension) => !dimensions[dimension]);
    const trimmedComment = comment.trim();
    if (missing || (trimmedComment.length > 0 && trimmedComment.length < 10)) {
      setValidationError(copy.required);
      return;
    }
    setValidationError(null);
    onSubmit({
      overallRating: overall,
      issueOccurred: issue,
      comment: trimmedComment || undefined,
      dimensions: Object.fromEntries(
        requiredDimensions.map((dimension) => [dimension, dimensions[dimension]!]),
      ) as Record<string, number>,
      expectedRevision,
    });
  };

  return (
    <Stack spacing={2} component="form" onSubmit={(event) => { event.preventDefault(); submit(); }}>
      <Typography variant="body2" color="text.secondary">{copy.noComparison}</Typography>
      <Stack component="fieldset" spacing={0.5} sx={{ border: 0, p: 0, m: 0 }}>
        <Typography component="legend" fontWeight={700}>{copy.overall}</Typography>
        <Rating
          value={overall}
          onChange={(_, value) => setOverall(value ?? 0)}
          getLabelText={(value) => value + ' de 5'}
          aria-required="true"
        />
      </Stack>
      {kind === 'store' && (
        <FormControlLabel
          control={<Checkbox checked={issue} onChange={(event) => setIssue(event.target.checked)} />}
          label={copy.issue}
        />
      )}
      {requiredDimensions.map((dimension) => (
        <Stack component="fieldset" spacing={0.5} sx={{ border: 0, p: 0, m: 0 }} key={dimension}>
          <Typography component="legend" variant="body2" fontWeight={700}>
            {dimensionNames[locale][dimension]}
          </Typography>
          <Rating
            value={dimensions[dimension] ?? 0}
            onChange={(_, value) => setDimensions((current) => ({ ...current, [dimension]: value ?? 0 }))}
            getLabelText={(value) => value + ' de 5'}
            aria-required="true"
          />
        </Stack>
      ))}
      <TextField
        label={copy.comment}
        value={comment}
        onChange={(event) => setComment(event.target.value)}
        multiline
        minRows={3}
        inputProps={{ maxLength: 3000 }}
        helperText={comment.trim().length > 0 && comment.trim().length < 10
          ? 'Mínimo 10 caracteres.'
          : comment.length + '/3000'}
      />
      {shownError && <Alert severity="error">{shownError}</Alert>}
      <Button type="submit" variant="contained" disabled={submitting}>
        {expectedRevision > 0 ? copy.edit : copy.submit}
      </Button>
    </Stack>
  );
}
