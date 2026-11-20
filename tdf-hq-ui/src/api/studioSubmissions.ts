import { API_BASE_URL, post } from './client';

export interface Submission {
  id: string;
  batch: number;
  position: number;
  name: string;
  state: 'pending' | 'accepted' | 'rejected' | 'expired';
  credit: string;
}

export const reviewRequest = <T>(action: string, body: object = {}) =>
  post<T>(`/cms/admin/studio-submissions/${action}`, body);

export async function intakeRequest<T>(action: string, body: object): Promise<T> {
  const response = await fetch(`${API_BASE_URL}/studio-submissions/${action}`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify(body),
    credentials: 'omit',
    referrerPolicy: 'no-referrer',
  });
  if (!response.ok) throw new Error(errorText(await response.text()));
  return response.json() as Promise<T>;
}

function errorText(body: string) {
  try {
    const result = JSON.parse(body) as { error?: string };
    if (result.error) return result.error;
  } catch {
    /* The reverse proxy can return a plain-text error. */
  }
  return 'No se pudo completar la solicitud. Reintenta.';
}

export function sendPhoto(
  file: File,
  position: number,
  token: string,
  receipt: string,
  progress: (percent: number) => void,
) {
  return new Promise<{ duplicate: boolean }>((resolve, reject) => {
    const request = new XMLHttpRequest();
    const data = new FormData();
    data.append('file', file);
    data.append('receipt', receipt);
    request.open('POST', `${API_BASE_URL}/studio-submissions/upload/${position}`);
    request.setRequestHeader('Authorization', 'Bearer ' + token);
    request.timeout = 180000;
    request.upload.onprogress = (event) => {
      if (event.lengthComputable) progress(Math.round((100 * event.loaded) / event.total));
    };
    request.onload = () => {
      if (request.status < 200 || request.status >= 300)
        return reject(new Error(errorText(request.responseText)));
      try {
        resolve(JSON.parse(request.responseText) as { duplicate: boolean });
      } catch {
        reject(new Error('Respuesta incompleta. Reintenta.'));
      }
    };
    request.onerror = request.ontimeout = () => reject(new Error('Revisa la conexión y reintenta.'));
    request.send(data);
  });
}
