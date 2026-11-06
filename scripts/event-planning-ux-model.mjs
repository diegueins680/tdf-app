// Finite UI callback model; the React tests separately exercise the real component.
export function verifyPosterRecoveryModel({ guardUpload = true, guardConfirmation = true, reuseUpload = true } = {}) {
  const queue = [{ phase: 'idle', active: true, cached: false, uploads: 0, depth: 0, trace: [] }];
  const seen = new Set();
  const violations = new Set();
  while (queue.length) {
    const state = queue.shift();
    const key = JSON.stringify([state.phase, state.active, state.cached, state.uploads, state.depth]);
    if (seen.has(key)) continue;
    seen.add(key);
    if (state.depth === 8) continue;
    const add = (action, changes) => queue.push({ ...state, ...changes, depth: state.depth + 1, trace: [...state.trace, action] });
    if (state.active) add('revoke session or unmount', { active: false });
    if ((state.phase === 'idle' || state.phase === 'failed') && state.active) {
      const reuse = state.cached && reuseUpload;
      if (state.cached && !reuse) violations.add('duplicate upload after a confirmation failure');
      add('save or retry', { phase: reuse ? 'confirming' : 'uploading', uploads: state.uploads + (reuse ? 0 : 1) });
    }
    if (state.phase === 'uploading') {
      if (!state.active && !guardUpload) violations.add('confirmation read after owner revocation');
      add('upload response', { phase: state.active || !guardUpload ? 'confirming' : 'stopped', cached: state.active || !guardUpload });
    }
    if (state.phase === 'confirming') {
      add('confirmation failure', { phase: 'failed' });
      if (!state.active && !guardConfirmation) violations.add('success feedback after owner revocation');
      add('confirmation response', { phase: state.active || !guardConfirmation ? 'saved' : 'stopped' });
    }
  }
  return { states: seen.size, depth: 8, violations: [...violations] };
}
