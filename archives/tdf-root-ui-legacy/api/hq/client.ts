const HQ_API_BASE = import.meta.env.VITE_API_BASE || 'http://localhost:8080';

type Primitive = string | number | boolean;
type QueryValue = Primitive | null | undefined | QueryValue[];

type ResponseType = 'json' | 'text' | 'blob' | 'arrayBuffer';

type RequestOptions = {
  params?: {
    path?: Record<string, Primitive | null | undefined>;
    query?: Record<string, QueryValue>;
  };
  body?: unknown;
  headers?: HeadersInit;
  parseAs?: ResponseType;
};

type FetchResult<TData> = {
  data: TData | undefined;
  error: unknown | undefined;
  response: Response;
};

type HttpMethod = 'GET' | 'POST' | 'PUT' | 'PATCH' | 'DELETE';

let authToken: string | null = null;
let unauthorizedHandler: (() => void) | null = null;

function isAbsoluteUrl(url: string) {
  return /^https?:\/\//i.test(url);
}

function normalizeBaseUrl(base: string) {
  if (!base) return '';
  return base.endsWith('/') ? base : `${base}/`;
}

function stripLeadingSlashes(path: string) {
  let start = 0;
  while (start < path.length && path[start] === '/') {
    start += 1;
  }
  return start > 0 ? path.slice(start) : path;
}

function buildPath(template: string, pathParams?: Record<string, Primitive | null | undefined>) {
  if (!pathParams) {
    return template;
  }
  return template.replace(/\{([^}]+)\}/g, (match, key) => {
    const value = pathParams[key];
    if (value === undefined || value === null) {
      throw new Error(`Missing value for path parameter "${key}"`);
    }
    return encodeURIComponent(String(value));
  });
}

function appendQuery(searchParams: URLSearchParams, key: string, value: QueryValue) {
  if (value == null) {
    return;
  }
  if (Array.isArray(value)) {
    for (const entry of value) {
      appendQuery(searchParams, key, entry);
    }
    return;
  }
  searchParams.append(key, String(value));
}

function resolveUrl(path: string, pathParams?: Record<string, Primitive | null | undefined>, query?: Record<string, QueryValue>) {
  const pathWithParams = buildPath(path, pathParams ?? undefined);
  const base = normalizeBaseUrl(HQ_API_BASE);
  const absolutePath = isAbsoluteUrl(pathWithParams);
  const trimmedPath = absolutePath ? pathWithParams : stripLeadingSlashes(pathWithParams);
  const absolute = absolutePath ? trimmedPath : `${base}${trimmedPath}`;
  const url = new URL(absolute);
  if (query && Object.keys(query).length > 0) {
    const params = new URLSearchParams(url.search);
    for (const [key, value] of Object.entries(query)) {
      appendQuery(params, key, value);
    }
    url.search = params.toString();
  }
  return url;
}

function isBodyInit(body: unknown): body is BodyInit {
  if (typeof Blob !== 'undefined' && body instanceof Blob) return true;
  if (typeof FormData !== 'undefined' && body instanceof FormData) return true;
  if (typeof URLSearchParams !== 'undefined' && body instanceof URLSearchParams) return true;
  if (typeof ReadableStream !== 'undefined' && body instanceof ReadableStream) return true;
  if (typeof ArrayBuffer !== 'undefined' && body instanceof ArrayBuffer) return true;
  if (typeof DataView !== 'undefined' && body instanceof DataView) return true;
  if (ArrayBuffer.isView(body)) return true;
  return false;
}

async function parseSuccessfulResponse<TData>(
  response: Response,
  parseAs: ResponseType = 'json',
): Promise<TData | undefined> {
  if (response.status === 204 || response.status === 205) {
    return undefined;
  }
  if (parseAs === 'blob') {
    return (await response.blob()) as unknown as TData;
  }
  if (parseAs === 'arrayBuffer') {
    return (await response.arrayBuffer()) as unknown as TData;
  }
  const contentType = response.headers.get('content-type') ?? '';
  if (parseAs === 'json' || contentType.includes('application/json')) {
    try {
      return (await response.json()) as TData;
    } catch {
      return undefined;
    }
  }
  if (parseAs === 'text' || contentType.startsWith('text/')) {
    const text = await response.text();
    return text as unknown as TData;
  }
  try {
    return (await response.json()) as TData;
  } catch {
    return undefined;
  }
}

async function extractErrorMessage(response: Response) {
  const contentType = response.headers.get('content-type') ?? '';
  if (contentType.includes('application/json')) {
    try {
      const data = await response.json();
      if (typeof data === 'string' && data.trim()) {
        return data;
      }
      if (data && typeof data === 'object') {
        for (const key of ['message', 'error', 'detail']) {
          const value = (data as Record<string, unknown>)[key];
          if (typeof value === 'string' && value.trim()) {
            return value;
          }
        }
        return JSON.stringify(data);
      }
    } catch {
      // ignore fallthrough to text
    }
  }
  try {
    const text = await response.text();
    if (text.trim()) {
      return text;
    }
  } catch {
    // ignore
  }
  return `HTTP ${response.status}`;
}

async function request<TData>(method: HttpMethod, path: string, options: RequestOptions = {}): Promise<FetchResult<TData>> {
  const { params, body, headers, parseAs } = options;
  const url = resolveUrl(path, params?.path, params?.query);

  const headerBag = new Headers(headers ?? {});
  if (authToken && !headerBag.has('Authorization')) {
    headerBag.set('Authorization', `Bearer ${authToken}`);
  }

  let requestBody: BodyInit | undefined;
  if (body != null) {
    if (isBodyInit(body)) {
      requestBody = body;
    } else if (typeof body === 'string') {
      requestBody = body;
      if (!headerBag.has('Content-Type')) {
        headerBag.set('Content-Type', 'application/json');
      }
    } else {
      requestBody = JSON.stringify(body);
      if (!headerBag.has('Content-Type')) {
        headerBag.set('Content-Type', 'application/json');
      }
    }
  }

  const response = await fetch(url.toString(), {
    method,
    headers: headerBag,
    credentials: 'include',
    body: requestBody,
  });

  if (response.status === 401 && unauthorizedHandler) {
    unauthorizedHandler();
  }

  if (!response.ok) {
    const message = await extractErrorMessage(response);
    return { data: undefined, error: new Error(message), response };
  }

  const data = await parseSuccessfulResponse<TData>(response, parseAs);
  return { data, error: undefined, response };
}

export const hqClient = {
  GET: <TData = unknown>(path: string, options?: RequestOptions) => request<TData>('GET', path, options),
  POST: <TData = unknown>(path: string, options?: RequestOptions) => request<TData>('POST', path, options),
  PUT: <TData = unknown>(path: string, options?: RequestOptions) => request<TData>('PUT', path, options),
  PATCH: <TData = unknown>(path: string, options?: RequestOptions) => request<TData>('PATCH', path, options),
  DELETE: <TData = unknown>(path: string, options?: RequestOptions) => request<TData>('DELETE', path, options),
};

export function setHqAuthToken(token: string | null) {
  authToken = token;
}

export function setHqUnauthorizedHandler(handler: (() => void) | null) {
  unauthorizedHandler = handler;
}

export async function unwrap<TData>(promise: Promise<FetchResult<TData>>) {
  const result = await promise;
  if (result.error) {
    if (result.error instanceof Error) {
      throw result.error;
    }
    throw new Error(String(result.error));
  }
  return result.data;
}

export async function unwrapRequired<TData>(promise: Promise<FetchResult<TData>>) {
  const data = await unwrap(promise);
  if (data == null) {
    throw new Error('Expected response payload but received empty body');
  }
  return data;
}

export async function unwrapVoid(promise: Promise<FetchResult<unknown>>) {
  const result = await promise;
  if (result.error) {
    if (result.error instanceof Error) {
      throw result.error;
    }
    throw new Error(String(result.error));
  }
}
