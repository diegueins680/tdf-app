import {
  GENERIC_REGISTRATION_ERROR,
  NETWORK_REGISTRATION_ERROR,
  describeCourseRegistrationError,
} from './courseRegistrationErrors';

const apiError = (message: string, status: number) => Object.assign(new Error(message), { status });

describe('describeCourseRegistrationError', () => {
  it('maps the legacy phone validation error to the phone field', () => {
    const view = describeCourseRegistrationError(apiError('phoneE164 inválido', 400));
    expect(view.field).toBe('phone');
    expect(view.message).toContain('0991234567');
    expect(view.message).not.toContain('phoneE164');
    expect(view.definitiveRejection).toBe(true);
  });

  it('maps email errors from both registration paths to the email field', () => {
    expect(describeCourseRegistrationError(apiError('email inválido', 400)).field).toBe('email');
    expect(describeCourseRegistrationError(apiError('email is invalid', 400)).field).toBe('email');
  });

  it('maps name problems to the name field without leaking the field id', () => {
    const view = describeCourseRegistrationError(apiError('fullName is too long', 400));
    expect(view.field).toBe('fullName');
    expect(view.message).toBe('Tu nombre es demasiado largo. Acórtalo un poco.');
  });

  it('maps seat, policy and missing-course conflicts to form-level copy', () => {
    const seats = describeCourseRegistrationError(apiError('No course seats remain', 409));
    expect(seats.field).toBeUndefined();
    expect(seats.message).toContain('Ya no quedan cupos');
    expect(describeCourseRegistrationError(
      apiError('This course has no approved active checkout price and policy', 409),
    ).message).toContain('aún no están abiertas');
    expect(describeCourseRegistrationError(apiError('Course not found', 404)).message)
      .toContain('ya no está disponible');
  });

  it('asks to reload the terms when they changed or were never shown', () => {
    const changed = describeCourseRegistrationError(apiError('Course terms changed; review the current terms and accept them again', 409));
    expect(changed).toMatchObject({ field: 'terms', termsChanged: true, definitiveRejection: true });
    const missing = describeCourseRegistrationError(apiError('Course terms version is required; review the current terms and accept them', 400));
    expect(missing).toMatchObject({ field: 'terms', termsChanged: true, definitiveRejection: true });
    expect(missing.message).not.toMatch(/version is required/i);
    // Simply not ticking the box is not a terms change.
    expect(describeCourseRegistrationError(apiError('Course checkout terms must be accepted before a seat can be held', 400)).termsChanged)
      .toBeUndefined();
  });

  it('retires the idempotency key after a mismatch conflict', () => {
    for (const message of [
      'This registration request was already saved with different details',
      'This registration changed after an earlier send. Review the saved registration before starting another submission.',
      'Idempotency key was already used for a different course checkout',
    ]) {
      expect(describeCourseRegistrationError(apiError(message, 409)).retireIdempotencyKey).toBe(true);
    }
  });

  it('treats network failures and timeouts as ambiguous and friendly', () => {
    const network = describeCourseRegistrationError(
      new Error('No se pudo conectar con el servicio. Revisa tu conexión e inténtalo de nuevo.'),
    );
    expect(network.message).toBe(NETWORK_REGISTRATION_ERROR);
    expect(network.definitiveRejection).toBe(false);
    expect(describeCourseRegistrationError(new TypeError('Failed to fetch')).message).toBe(NETWORK_REGISTRATION_ERROR);
    expect(describeCourseRegistrationError(apiError('La solicitud tardó demasiado.', 408)).message)
      .toBe(NETWORK_REGISTRATION_ERROR);
  });

  it('never surfaces raw server text for unknown failures', () => {
    const view = describeCourseRegistrationError(apiError('Could not resolve registration request', 500));
    expect(view.message).toBe(GENERIC_REGISTRATION_ERROR);
    expect(view.definitiveRejection).toBe(false);
    expect(describeCourseRegistrationError(new Error('provider unavailable')).message).toBe(GENERIC_REGISTRATION_ERROR);
  });
});
