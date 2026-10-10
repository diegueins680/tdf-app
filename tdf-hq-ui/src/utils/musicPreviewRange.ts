export function musicPreviewRangeError(start: number | null, duration: number | null, sourceDuration?: number | null): string | null {
  if (start === null && duration === null) return null;
  if (start !== null && (!Number.isSafeInteger(start) || start < 0)) return 'El inicio debe ser un entero no negativo en milisegundos.';
  if (duration === null || !Number.isSafeInteger(duration) || duration <= 0) return 'Indica una duración entera positiva en milisegundos.';
  if (sourceDuration != null && ((start ?? 0) >= sourceDuration || duration > sourceDuration - (start ?? 0))) {
    return 'El preview debe quedar dentro de la duración técnica de la pista.';
  }
  return null;
}
