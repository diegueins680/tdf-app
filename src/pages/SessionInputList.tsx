import * as React from 'react';
import { useQuery } from '@tanstack/react-query';
import { fetchInputList, downloadInputListPdf } from '../api/inputList';

type Props = {
  sessionId?: number;
};

export default function SessionInputList({ sessionId = 1 }: Props) {
  const q = useQuery({
    queryKey: ['input-list', sessionId],
    queryFn: () => fetchInputList(sessionId),
  });

  if (q.isLoading) return <div>Cargando…</div>;
  if (q.isError) return <div>Error cargando input list</div>;

  return (
    <div style={{ padding: 16 }}>
      <h2>Input List — Holger Quiñónez</h2>
      <button onClick={() => downloadInputListPdf(sessionId)}>Descargar PDF</button>
      <table style={{ width: '100%', borderCollapse: 'collapse', marginTop: 12 }}>
        <thead>
          <tr style={{ background: '#c4161c', color: '#fff' }}>
            <th>#</th>
            <th>Fuente</th>
            <th>Mic/DI</th>
            <th>Medusa</th>
            <th>Preamp</th>
            <th>Interfaz</th>
            <th>DAW</th>
            <th>Notas</th>
          </tr>
        </thead>
        <tbody>
          {q.data!.map((r) => (
            <tr key={r.id} style={{ background: r.ch % 2 ? '#f6f6f6' : '#fff' }}>
              <td>{r.ch}</td>
              <td>{r.source}</td>
              <td>{r.micDi}</td>
              <td>{r.medusa ?? ''}</td>
              <td>{r.preamp}</td>
              <td>{r.interfaceChan}</td>
              <td>{r.dawCh}</td>
              <td>{r.notes ?? ''}</td>
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  );
}

