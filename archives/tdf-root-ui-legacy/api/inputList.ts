export type InputRow = {
  id: number;
  ch: number;
  source: string;
  micDi: string;
  medusa?: string | null;
  preamp: string;
  interfaceChan: string;
  dawCh: number;
  notes?: string | null;
};

const API = import.meta.env.VITE_API_BASE || 'http://localhost:8080';

export async function fetchInputList(sessionId: number): Promise<InputRow[]> {
  const r = await fetch(`${API}/sessions/${sessionId}/input-list`);
  if (!r.ok) throw new Error('No se pudo cargar el input list');
  return r.json();
}

export function downloadInputListPdf(sessionId: number) {
  const url = `${API}/sessions/${sessionId}/input-list.pdf`;
  const a = document.createElement('a');
  a.href = url;
  a.setAttribute('download', 'input-list.pdf'); // el servidor ya manda Content-Disposition
  document.body.appendChild(a);
  a.click();
  a.remove();
}

