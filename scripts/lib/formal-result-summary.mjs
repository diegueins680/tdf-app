// Count admitted executions, including both negative-control helpers. This does
// not replace the runner's exit/property checks or source freshness validation.
export function tlcSummary(log) {
  const results = [];
  const seen = new Set();
  for (const line of log.split('\n')) {
    if (!line.startsWith('TDF_TLC_RESULT')) continue;
    const match = /^TDF_TLC_RESULT (positive|negative) ([A-Za-z0-9_-]+\.tla) ([A-Za-z0-9_-]+\.cfg)$/.exec(line);
    if (!match) throw new Error('Malformed TLC result marker');
    const [, kind, module, config] = match;
    const key = `${module}/${config}`;
    if (seen.has(key)) throw new Error(`Duplicate TLC result: ${key}`);
    seen.add(key);
    results.push({ kind, module, config });
  }
  return { positive: results.filter(x => x.kind === 'positive').length,
    negative: results.filter(x => x.kind === 'negative').length, configurations: results };
}
