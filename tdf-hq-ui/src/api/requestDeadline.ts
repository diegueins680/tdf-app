/** One deadline covers receiving headers and consuming the response body. */
export class RequestTimeoutError extends Error {
  constructor() {
    super('Request deadline exceeded');
    this.name = 'RequestTimeoutError';
  }
}

export async function fetchWithDeadline<T>(
  input: RequestInfo | URL,
  init: RequestInit,
  readResponse: (response: Response) => Promise<T>,
): Promise<T> {
  const controller = new AbortController();
  const callerSignal = init.signal;
  const abortForCaller = () => controller.abort(callerSignal?.reason);
  if (callerSignal?.aborted) {
    callerSignal.throwIfAborted();
  }
  callerSignal?.addEventListener('abort', abortForCaller, { once: true });
  let timedOut = false;
  const timeoutId = setTimeout(() => {
    timedOut = true;
    controller.abort();
  }, 30_000);

  try {
    const response = await fetch(input, { ...init, signal: controller.signal });
    const result = await readResponse(response);
    // Do not publish a result if cancellation raced with completion.
    controller.signal.throwIfAborted();
    return result;
  } catch (error) {
    if (timedOut && !callerSignal?.aborted) throw new RequestTimeoutError();
    throw error;
  } finally {
    clearTimeout(timeoutId);
    callerSignal?.removeEventListener('abort', abortForCaller);
  }
}
