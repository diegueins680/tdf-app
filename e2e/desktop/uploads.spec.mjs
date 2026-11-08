import { expect, test } from '@playwright/test';
import { launchShell } from './helpers/shell.mjs';
import { RELEASE, STAFF_ACCOUNT, createApiFixture } from './helpers/api-fixture.mjs';

// Flow 1: upload a master by drag and drop inside the shell, lose the
// connection mid-upload, retry without duplicating the upload session, keep the
// draft, then validate and submit the release. The transition retry replays the
// same Idempotency-Key. API and storage are synthetic.

test('drag-and-drop master upload survives a network cut and publication replays idempotently', async ({ baseURL }) => {
  test.setTimeout(150_000);
  // The release Studio route is a label/staff feature (label.releases); the
  // synthetic staff account manages the draft on behalf of artist party 101.
  const api = createApiFixture({ account: STAFF_ACCOUNT });
  const shell = await launchShell({ appOrigin: baseURL });
  try {
    await api.install(shell.app.context(), baseURL);
    const { window } = shell;
    await shell.openLogin();
    await window.getByLabel('Usuario o correo').fill('staff@example.invalid');
    await window.getByLabel('Contraseña', { exact: false }).first().fill('secreta-larga-1');
    await window.getByRole('button', { name: 'Ingresar', exact: true }).click();
    await expect(window).not.toHaveURL(/\/login/);

    await window.evaluate(({ releaseId, versionId }) => {
      window.history.pushState({}, '', `/label/releases/${releaseId}/versions/${versionId}?artistPartyId=101`);
      window.dispatchEvent(new PopStateEvent('popstate'));
    }, RELEASE);
    const title = window.getByRole('textbox', { name: 'Título', exact: true }).first();
    await expect(title).toHaveValue('Borrador de escritorio', { timeout: 30_000 });
    await title.fill('Borrador de escritorio editado en la app');

    // First attempt: the direct part upload to storage loses the connection.
    api.failNext((method, path) => method === 'PUT' && path.startsWith('/synthetic-storage/'), 'network');
    const dropzone = window.getByTestId('upload-dropzone-master_audio');
    const transfer = await window.evaluateHandle(() => {
      const data = new DataTransfer();
      data.items.add(new File([new Uint8Array(64 * 1024).fill(7)], 'master-escritorio.wav', { type: 'audio/wav', lastModified: 1_700_000_000_000 }));
      return data;
    });
    await dropzone.dispatchEvent('dragover', { dataTransfer: transfer });
    await dropzone.dispatchEvent('drop', { dataTransfer: transfer });
    await expect(window.getByText('La parte no pudo llegar al almacenamiento.')).toBeVisible({ timeout: 30_000 });
    expect(api.writesTo(`/music/releases/${RELEASE.releaseId}/versions/${RELEASE.versionId}/uploads`)).toHaveLength(1);

    // Retry: same file identity, same Idempotency-Key, single upload session.
    await window.getByRole('button', { name: 'Reintentar pista 1' }).click();
    await expect(window.getByText('Carga confirmada; procesamiento en cola.')).toBeVisible({ timeout: 60_000 });
    const sessions = api.writesTo(`/music/releases/${RELEASE.releaseId}/versions/${RELEASE.versionId}/uploads`);
    expect(sessions).toHaveLength(2);
    expect(sessions[0].idempotencyKey).toMatch(/^music-upload-/);
    expect(sessions[1].idempotencyKey).toBe(sessions[0].idempotencyKey);
    expect(sessions[1].body.expectedSha256).toBe(sessions[0].body.expectedSha256);
    expect(api.state.uploadSessions.size).toBe(1);
    expect(api.writesTo('/music/uploads/upload-1/confirm')).toHaveLength(1);
    expect(api.state.uploadedParts.get('upload-1')).toHaveLength(1);
    // The draft edit made before the upload reached the server and is still on screen.
    const drafts = api.writesTo(`/music/releases/${RELEASE.releaseId}/versions/${RELEASE.versionId}/draft`);
    expect(drafts.some((entry) => entry.body.title === 'Borrador de escritorio editado en la app')).toBe(true);
    await expect(title).toHaveValue('Borrador de escritorio editado en la app');
    await expect(window.getByText(/master_audio: queued/)).toBeVisible();

    // Validation and submission: the first transition attempt loses the
    // connection; the retry carries the same Idempotency-Key and the server
    // records one lifecycle action.
    await window.locator('input[type="checkbox"]').first().check();
    await window.getByRole('button', { name: 'Aceptar y validar' }).click();
    await expect(window.getByText(/Validación editorial completa/)).toBeVisible({ timeout: 30_000 });
    api.failNext((method, path) => method === 'POST' && path.endsWith('/transition'), 'network');
    await window.getByRole('button', { name: 'Enviar a revisión' }).click();
    await expect(window.getByText(/No se pudo conectar con el servicio/)).toBeVisible({ timeout: 30_000 });
    await window.getByRole('button', { name: 'Enviar a revisión' }).click();
    await expect(window.getByText('Estado actualizado: in_review.')).toBeVisible({ timeout: 30_000 });
    // The first attempt was cut before the server could record it; both
    // attempts reached the wire with the same Idempotency-Key and the server
    // recorded exactly one lifecycle action.
    const transitionAttempts = api.requestsTo(`/music/releases/${RELEASE.releaseId}/versions/${RELEASE.versionId}/transition`);
    expect(transitionAttempts).toHaveLength(2);
    expect(transitionAttempts[0].headers['idempotency-key']).toMatch(/^music-transition-/);
    expect(transitionAttempts[1].headers['idempotency-key']).toBe(transitionAttempts[0].headers['idempotency-key']);
    const transitions = api.writesTo(`/music/releases/${RELEASE.releaseId}/versions/${RELEASE.versionId}/transition`);
    expect(transitions).toHaveLength(1);
    expect(transitions[0].body.targetState).toBe('ready_for_review');
    expect(api.state.transitions.size).toBe(1);
    expect(api.state.studio.state).toBe('in_review');
  } finally { await shell.close(); }
});
