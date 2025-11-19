import { useParams, useSearchParams } from 'react-router-dom';
import { useState } from 'react';

export default function InscripcionPage() {
  const { slug } = useParams();
  const [sp] = useSearchParams();
  const leadId = sp.get('lead') || '';
  const token = sp.get('t') || '';

  const [name, setName] = useState('');
  const [email, setEmail] = useState('');
  const [done, setDone] = useState(false);
  const [busy, setBusy] = useState(false);
  const [error, setError] = useState('');

  async function submit(e: React.FormEvent) {
    e.preventDefault();
    setBusy(true);
    setError('');
    try {
      const res = await fetch(`${import.meta.env.VITE_API_BASE}/api/leads/${leadId}/complete`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ token, name, email }),
      });
      if (res.ok) {
        setDone(true);
      } else {
        // Only show user-friendly messages, not raw backend errors
        const userMessage = res.status === 400
          ? 'Por favor verifica los datos ingresados.'
          : 'Error al enviar el formulario. Por favor intenta nuevamente.';
        setError(userMessage);
      }
    } catch (err) {
      setError('Error de conexión. Por favor verifica tu conexión a internet e intenta nuevamente.');
    } finally {
      setBusy(false);
    }
  }

  if (!leadId || !token) {
    return (
      <main style={{ maxWidth: 580, margin: '24px auto', padding: '0 16px' }}>
        <h1>Invitación inválida o incompleta</h1>
        <p>Vuelve al mensaje de WhatsApp y abre nuevamente el enlace.</p>
      </main>
    );
  }

  if (done) {
    return (
      <main style={{ maxWidth: 580, margin: '24px auto', padding: '0 16px' }}>
        <h1>¡Listo! 🎉</h1>
        <p>
          Hemos recibido tus datos para <strong>{slug?.replace(/-/g, ' ')}</strong>. Te contactaremos por WhatsApp y
          correo con los siguientes pasos.
        </p>
      </main>
    );
  }

  return (
    <main style={{ maxWidth: 580, margin: '24px auto', padding: '0 16px' }}>
      <h1>Inscripción — {slug?.replace(/-/g, ' ')}</h1>
      {/* TODO: Fetch course details dynamically from backend course_edition table */}
      <p>
        Presencial (cupo 10). Sábados 13, 20, 27 dic 2025 y 3 ene 2026. 16 h totales. DAWs: Logic y Luna. Incluye
        grabaciones y certificado. Precio: $150 USD. Descuento por referidos.
      </p>

      {error && (
        <div style={{ padding: '12px', marginBottom: '16px', backgroundColor: '#fee', border: '1px solid #fcc', borderRadius: '4px', color: '#c00' }}>
          {error}
        </div>
      )}

      <form onSubmit={submit}>
        <label>Nombre</label>
        <input value={name} onChange={(e) => setName(e.target.value)} required style={{ display: 'block', width: '100%' }} />
        <label style={{ marginTop: 12 }}>Correo</label>
        <input
          value={email}
          onChange={(e) => setEmail(e.target.value)}
          type="email"
          required
          style={{ display: 'block', width: '100%' }}
        />
        <button type="submit" disabled={busy} style={{ marginTop: 16 }}>
          {busy ? 'Enviando…' : 'Enviar'}
        </button>
      </form>
    </main>
  );
}
