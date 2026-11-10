'use strict';
// Minimal append-only diagnostics log in the per-user logs directory. No
// request bodies, tokens or page content are ever written here.
const fs = require('node:fs');
const path = require('node:path');

function createLogger(directory) {
  let stream = null;
  try {
    fs.mkdirSync(directory, { recursive: true });
    stream = fs.createWriteStream(path.join(directory, 'tdf-desktop.log'), { flags: 'a' });
  } catch {
    stream = null;
  }
  const write = (level, message) => {
    const line = `${new Date().toISOString()} ${level} ${String(message).replace(/\r?\n/g, ' ')}\n`;
    if (stream) stream.write(line);
    if (level === 'ERROR') process.stderr.write(line);
  };
  return {
    info: (message) => write('INFO', message),
    warn: (message) => write('WARN', message),
    error: (message) => write('ERROR', message),
    directory,
  };
}

module.exports = { createLogger };
