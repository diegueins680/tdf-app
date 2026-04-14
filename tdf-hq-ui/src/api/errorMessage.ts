const isJsonContentType = (contentType: string): boolean => /[/+]json(?:;|$)/i.test(contentType);

const looksLikeJsonPayload = (value: string): boolean => {
  const trimmed = value.trim();
  return trimmed.startsWith('{') || trimmed.startsWith('[');
};

const normalizeErrorMessage = (value: unknown): string | null => {
  if (typeof value !== 'string') return null;
  const trimmed = value.trim();
  return trimmed === '' ? null : trimmed;
};

const extractErrorCandidate = (value: unknown): string | null => {
  const direct = normalizeErrorMessage(value);
  if (direct) return direct;

  if (Array.isArray(value)) {
    for (const item of value) {
      const candidate = extractErrorCandidate(item);
      if (candidate) return candidate;
    }
    return null;
  }

  if (!value || typeof value !== 'object') return null;

  const payload = value as Record<string, unknown>;
  const priorityKeys = ['message', 'error', 'detail', 'title', 'reason', 'description', 'errors'];
  for (const key of priorityKeys) {
    const candidate = extractErrorCandidate(payload[key]);
    if (candidate) return candidate;
  }

  for (const candidateValue of Object.values(payload)) {
    const candidate = extractErrorCandidate(candidateValue);
    if (candidate) return candidate;
  }

  return null;
};

export const extractErrorDetails = (rawBody: string, contentType: string): string => {
  const trimmedBody = rawBody.trim();
  if (trimmedBody === '') return '';
  if (!isJsonContentType(contentType) && !looksLikeJsonPayload(trimmedBody)) return trimmedBody;

  try {
    const parsed = JSON.parse(trimmedBody) as unknown;
    const candidate = extractErrorCandidate(parsed);
    if (candidate) return candidate;
  } catch {
    return trimmedBody;
  }

  return trimmedBody;
};
