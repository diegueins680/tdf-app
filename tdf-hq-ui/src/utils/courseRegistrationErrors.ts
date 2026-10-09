import { PHONE_EXAMPLE_HINT } from './phone';

export type CourseRegistrationField = 'fullName' | 'email' | 'phone' | 'howHeard' | 'terms';

export interface CourseRegistrationErrorView {
  /** When set, the message belongs next to this field instead of the form-level alert. */
  field?: CourseRegistrationField;
  message: string;
  /**
   * The server already holds a request under the current Idempotency-Key with
   * different details, so the next attempt must use a fresh key.
   */
  retireIdempotencyKey: boolean;
  /**
   * The server definitively rejected the request (nothing was saved), so a
   * corrected payload may safely use a fresh key. Ambiguous failures
   * (network, timeout, 5xx) keep the key so an identical retry is deduplicated.
   */
  definitiveRejection: boolean;
}

export const GENERIC_REGISTRATION_ERROR =
  'No pudimos registrar tu inscripción. Intenta de nuevo en unos minutos o escríbenos por WhatsApp.';
export const NETWORK_REGISTRATION_ERROR =
  'No pudimos conectarnos. Revisa tu conexión a internet e intenta de nuevo.';

const readStatus = (error: unknown): number | null => {
  if (!error || typeof error !== 'object') return null;
  const status = (error as { status?: unknown }).status;
  return typeof status === 'number' && Number.isFinite(status) ? status : null;
};

const readMessage = (error: unknown): string => {
  if (error instanceof Error) return error.message;
  if (typeof error === 'string') return error;
  return '';
};

const describeFieldProblem = (message: string, subject: string, fallback: string) => {
  if (/too long/i.test(message)) return `${subject} es demasiado largo. Acórtalo un poco.`;
  if (/unsupported characters/i.test(message)) return `${subject} tiene caracteres no permitidos.`;
  return fallback;
};

/**
 * Map a registration failure to Spanish copy a buyer can act on. Raw server
 * text, status codes and stack traces are never returned.
 */
export function describeCourseRegistrationError(error: unknown): CourseRegistrationErrorView {
  const status = readStatus(error);
  const message = readMessage(error);
  const lower = message.toLowerCase();
  const view = (
    copy: string,
    options: Partial<Omit<CourseRegistrationErrorView, 'message'>> = {},
  ): CourseRegistrationErrorView => ({
    message: copy,
    retireIdempotencyKey: options.retireIdempotencyKey ?? false,
    definitiveRejection: options.definitiveRejection ?? false,
    ...(options.field ? { field: options.field } : {}),
  });

  if (status === null) {
    if (/conectar|network|failed to fetch|load failed|conexi[oó]n/i.test(message)) {
      return view(NETWORK_REGISTRATION_ERROR);
    }
    return view(GENERIC_REGISTRATION_ERROR);
  }

  if (status === 408 || status === 0) return view(NETWORK_REGISTRATION_ERROR);
  if (status === 429) {
    return view('Recibimos varios intentos seguidos. Espera un minuto e intenta de nuevo.');
  }

  if (status === 400 || status === 422) {
    if (lower.includes('phone')) {
      return view(`Revisa tu número de WhatsApp. ${PHONE_EXAMPLE_HINT}.`, {
        field: 'phone',
        definitiveRejection: true,
      });
    }
    if (lower.includes('email') || lower.includes('correo')) {
      return view('Revisa tu correo: debe tener el formato nombre@dominio.com.', {
        field: 'email',
        definitiveRejection: true,
      });
    }
    if (lower.includes('fullname') || lower.includes('nombre')) {
      return view(describeFieldProblem(message, 'Tu nombre', 'Escribe tu nombre completo.'), {
        field: 'fullName',
        definitiveRejection: true,
      });
    }
    if (lower.includes('howheard')) {
      return view(describeFieldProblem(message, 'Tu respuesta', 'Revisa cómo te enteraste del curso.'), {
        field: 'howHeard',
        definitiveRejection: true,
      });
    }
    if (lower.includes('terms')) {
      return view('Debes aceptar los términos y la política de cancelación para continuar.', {
        field: 'terms',
        definitiveRejection: true,
      });
    }
    if (lower.includes('idempotency')) {
      return view(GENERIC_REGISTRATION_ERROR, { retireIdempotencyKey: true, definitiveRejection: true });
    }
    return view(
      'Algunos datos no son válidos. Revisa el formulario o escríbenos por WhatsApp.',
      { definitiveRejection: true },
    );
  }

  if (status === 404) {
    return view(
      'Este curso ya no está disponible para inscripciones en línea. Escríbenos por WhatsApp para ver otras fechas.',
      { definitiveRejection: true },
    );
  }

  if (status === 409) {
    if (/seat|cupo/i.test(message)) {
      return view(
        'Ya no quedan cupos para esta fecha. Escríbenos por WhatsApp y te avisamos si se libera uno.',
        { definitiveRejection: true },
      );
    }
    if (/policy|price|checkout price/i.test(message) && !/already exists/i.test(message)) {
      return view(
        'Las inscripciones en línea para este curso aún no están abiertas. Escríbenos por WhatsApp para reservar tu cupo.',
        { definitiveRejection: true },
      );
    }
    if (/already exists/i.test(message)) {
      return view('Tu solicitud ya está registrada. Intenta de nuevo para ver su estado.');
    }
    if (/different details|changed after an earlier send|different course checkout|conflicts with an existing request|idempotency/i.test(message)) {
      return view(
        'Tus datos cambiaron desde el intento anterior. Revisa el formulario y vuelve a enviarlo.',
        { retireIdempotencyKey: true, definitiveRejection: true },
      );
    }
    return view(GENERIC_REGISTRATION_ERROR, { definitiveRejection: true });
  }

  if (status >= 400 && status < 500) return view(GENERIC_REGISTRATION_ERROR, { definitiveRejection: true });
  return view(GENERIC_REGISTRATION_ERROR);
}
