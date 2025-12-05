const systemPrompt = `
You are a TidalCycles code generator.
- Reply ONLY with TidalCycles code, no prose or markdown.
- Keep patterns short, loopable, and safe to evaluate.
- Use d1/d2/d3/d4, stack, sound, n, note, cps/bps, hush. Avoid file I/O or shell commands.
- Prefer concise percussive or melodic patterns; avoid very long sequences.
`.trim();

const safePrefixes = ['d1', 'd2', 'd3', 'd4', 'hush', 'bps', 'cps', 'setcps', 'once', 'solo', 'unsolo', 'all', 'xfade', 'xfadeIn'];

const allowedLine = (line: string) => {
  const trimmed = line.trim();
  if (!trimmed) return false;
  return safePrefixes.some((p) => trimmed.startsWith(p));
};

export const extractTidalCode = (text: string): string | null => {
  const fenceMatch = text.match(/```(?:tidal)?\s*([\s\S]*?)```/i);
  const body = fenceMatch ? fenceMatch[1] ?? '' : text;
  const lines = body
    .split('\n')
    .map((l) => l.replace(/[\r\t]/g, '').trimEnd())
    .filter((l) => !l.startsWith('--') && !l.startsWith('//'));
  const filtered = lines.filter(allowedLine);
  if (filtered.length === 0) return null;
  return filtered.slice(0, 12).join('\n');
};

export interface TidalAgentConfig {
  apiUrl: string;
  apiKey: string;
  model: string;
}

export const buildDefaultConfig = (): { config: TidalAgentConfig | null; error?: string } => {
  const apiUrl =
    import.meta.env['VITE_TIDAL_AGENT_API_URL'] ||
    import.meta.env['VITE_OPENAI_API_URL'] ||
    'https://api.openai.com/v1/chat/completions';
  const apiKey = import.meta.env['VITE_TIDAL_AGENT_API_KEY'] || import.meta.env['VITE_OPENAI_API_KEY'] || '';
  const model = import.meta.env['VITE_TIDAL_AGENT_MODEL'] || 'gpt-4o-mini';
  if (!apiKey) {
    return { config: null, error: 'Falta configurar VITE_TIDAL_AGENT_API_KEY (o VITE_OPENAI_API_KEY).' };
  }
  return { config: { apiUrl, apiKey, model } };
};

export const tidalAgentRequest = async (prompt: string, cfg: TidalAgentConfig): Promise<string> => {
  const body = {
    model: cfg.model,
    messages: [
      { role: 'system', content: systemPrompt },
      { role: 'user', content: prompt },
    ],
    temperature: 0.6,
    max_tokens: 300,
  };
  const resp = await fetch(cfg.apiUrl, {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
      Authorization: `Bearer ${cfg.apiKey}`,
    },
    body: JSON.stringify(body),
  });
  if (!resp.ok) {
    const text = await resp.text();
    throw new Error(`API ${resp.status}: ${text.slice(0, 200)}`);
  }
  const json = await resp.json();
  const content = json?.choices?.[0]?.message?.content;
  if (!content || typeof content !== 'string') {
    throw new Error('Respuesta vacía o inválida del modelo.');
  }
  return content;
};

export const generateTidalCode = (prompt: string, mood?: string): { code: string } => {
  const base = prompt.toLowerCase();
  const isTechno = base.includes('techno') || (mood ?? '').toLowerCase().includes('club');
  const isAmbient = base.includes('ambient') || base.includes('drone') || (mood ?? '').toLowerCase().includes('ambient');
  if (isAmbient) {
    return {
      code: ['d1 $ slow 2 $ sound "pad*2 mist"', 'd2 $ every 4 (degradeBy 0.4) $ n "c3 e3 g3" # sound "sine"'].join('\n'),
    };
  }
  if (isTechno) {
    return {
      code: ['d1 $ stack [sound "bd*4", sound "hh*8", every 4 (fast 2) $ sound "cp*2"]', 'd2 $ n "0 3 7 5" # sound "bass" # gain 0.8'].join('\n'),
    };
  }
  return {
    code: ['d1 $ stack [sound "bd sd", sound "hh*4" # gain 0.7]', 'd2 $ n "0 2 4 7" # sound "arp" # cut 0.7'].join('\n'),
  };
};
