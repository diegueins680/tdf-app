'use strict';
// Last playback report from the hosted application, sanitized for native menus.
const OWNERS = new Set(['music', 'radio']);
const STATUSES = new Set(['idle', 'loading', 'playing', 'paused', 'error']);
const COMMANDS = new Set(['play', 'pause', 'toggle', 'next', 'previous']);
const MAX_TEXT = 200;

const IDLE = Object.freeze({ owner: null, status: 'idle', title: null, artist: null, canNext: false, canPrevious: false });

function cleanText(value) {
  if (typeof value !== 'string') return null;
  const text = value.replace(/[\u0000-\u001f\u007f]/g, ' ').trim();
  return text ? text.slice(0, MAX_TEXT) : null;
}

/** Returns a frozen report or null when the payload is not a playback report. */
function sanitizeReport(raw) {
  if (!raw || typeof raw !== 'object') return null;
  const owner = raw.owner === null ? null : OWNERS.has(raw.owner) ? raw.owner : undefined;
  if (owner === undefined || !STATUSES.has(raw.status)) return null;
  return Object.freeze({
    owner,
    status: raw.status,
    title: cleanText(raw.title),
    artist: cleanText(raw.artist),
    canNext: raw.canNext === true,
    canPrevious: raw.canPrevious === true,
  });
}

function isCommand(value) { return typeof value === 'string' && COMMANDS.has(value); }

/** Menu model derived from a report; label text is what the OS menu shows. */
function describeForMenu(report) {
  const state = report ?? IDLE;
  const hasOwner = state.owner !== null && state.status !== 'idle';
  const playing = state.status === 'playing' || state.status === 'loading';
  const subject = [state.title, state.artist].filter(Boolean).join(' · ');
  return {
    nowPlaying: hasOwner ? (subject || (state.owner === 'radio' ? 'Radio TDF' : 'Reproductor TDF')) : 'Nada en reproducción',
    toggleLabel: playing ? 'Pausar' : 'Reproducir',
    toggleEnabled: hasOwner,
    nextEnabled: hasOwner && state.canNext,
    previousEnabled: hasOwner && state.canPrevious,
    owner: hasOwner ? state.owner : null,
  };
}

class PlaybackState {
  constructor() { this.report = IDLE; this.listeners = new Set(); }
  update(raw) {
    const report = sanitizeReport(raw);
    if (!report) return false;
    this.report = report;
    for (const listener of this.listeners) listener(report);
    return true;
  }
  reset() { this.update(IDLE); }
  get() { return this.report; }
  onChange(listener) { this.listeners.add(listener); return () => this.listeners.delete(listener); }
}

module.exports = { COMMANDS, IDLE, PlaybackState, describeForMenu, isCommand, sanitizeReport };
