import { post } from '../api/client';
import { env } from './env';

const safePrefixes = ['d1', 'd2', 'd3', 'd4', 'hush', 'bps', 'cps', 'setcps', 'once', 'solo', 'unsolo', 'all', 'xfade', 'xfadeIn'];

const allowedLine = (line: string) => {
  const trimmed = line.trim();
  if (!trimmed) return false;
  return safePrefixes.some((p) => trimmed.startsWith(p));
};

export const extractTidalCode = (text: string): string | null => {
  const fenceMatch = /```(?:tidal)?\s*([\s\S]*?)```/i.exec(text);
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
  model?: string;
}

export const buildDefaultConfig = (): TidalAgentConfig => {
  const model = env.read('VITE_TIDAL_AGENT_MODEL');
  const trimmed = model?.trim();
  const normalized = trimmed === '' ? undefined : trimmed;
  return { model: normalized };
};

export const tidalAgentRequest = async (prompt: string, cfg: TidalAgentConfig): Promise<string> => {
  const payload = await post<{ content: string }>('/tidal-agent', {
    prompt,
    model: cfg.model,
  });
  const content = payload?.content;
  if (!content || typeof content !== 'string') {
    throw new Error('Respuesta vacía o inválida del modelo.');
  }
  return content;
};

const deriveTempo = (prompt: string, mood?: string) => {
  const text = `${prompt} ${mood ?? ''}`.toLowerCase();
  if (text.includes('ambient') || text.includes('drone')) return 78;
  if (text.includes('techno') || text.includes('club') || text.includes('rave')) return 132;
  if (text.includes('house')) return 124;
  if (text.includes('reggaeton') || text.includes('dembow')) return 96;
  if (text.includes('trap') || text.includes('hip hop')) return 88;
  return 120;
};

const deriveTags = (prompt: string, mood?: string) => {
  const text = `${prompt} ${mood ?? ''}`.toLowerCase();
  const tags: string[] = [];
  if (text.includes('ambient') || text.includes('drone')) tags.push('ambient');
  if (text.includes('techno') || text.includes('club')) tags.push('techno');
  if (text.includes('house')) tags.push('house');
  if (text.includes('reggaeton') || text.includes('dembow')) tags.push('latin');
  if (text.includes('trap') || text.includes('hip hop')) tags.push('trap');
  if (text.includes('melod')) tags.push('melódico');
  if (text.includes('perc')) tags.push('percusión');
  if (tags.length === 0) tags.push('experimental');
  return tags.slice(0, 3);
};

export const generateTidalCode = (prompt: string, mood?: string): { code: string; tempo: number; tags: string[] } => {
  const base = prompt.toLowerCase();
  const isTechno = base.includes('techno') || (mood ?? '').toLowerCase().includes('club');
  const isAmbient = base.includes('ambient') || base.includes('drone') || (mood ?? '').toLowerCase().includes('ambient');
  const tempo = deriveTempo(prompt, mood);
  const tags = deriveTags(prompt, mood);
  if (isAmbient) {
    return {
      code: ['d1 $ slow 2 $ sound "pad*2 mist"', 'd2 $ every 4 (degradeBy 0.4) $ n "c3 e3 g3" # sound "sine"'].join('\n'),
      tempo,
      tags,
    };
  }
  if (isTechno) {
    return {
      code: ['d1 $ stack [sound "bd*4", sound "hh*8", every 4 (fast 2) $ sound "cp*2"]', 'd2 $ n "0 3 7 5" # sound "bass" # gain 0.8'].join('\n'),
      tempo,
      tags,
    };
  }
  return {
    code: ['d1 $ stack [sound "bd sd", sound "hh*4" # gain 0.7]', 'd2 $ n "0 2 4 7" # sound "arp" # cut 0.7'].join('\n'),
    tempo,
    tags,
  };
};
