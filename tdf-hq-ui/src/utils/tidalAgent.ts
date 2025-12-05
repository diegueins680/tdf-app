export type GeneratedTidal = {
  code: string;
  tempo: number;
  tags: string[];
};

type Rule = {
  keywords: string[];
  tempo: number;
  perc: string;
  bass: string;
  lead: string;
  fx?: string;
  tags: string[];
};

const RULES: Rule[] = [
  {
    keywords: ['ambient', 'downtempo', 'paisaje', 'drone', 'chill'],
    tempo: 78,
    perc: "bd(2,8) ~ ~ cp ~",
    bass: "slow 2 $ n \"0 0 3 5\" # s \"sine\" # cutoff 800 # resonance 0.2",
    lead: "slow 3 $ n (scale \"aeolian\" \"0 2 4 7\" + octave 4) # s \"pad\" # room 0.5 # size 0.6",
    fx: "# orbit 1 # gain 0.9",
    tags: ['ambient', 'pad'],
  },
  {
    keywords: ['ukg', 'garage', 'bassline', 'break', 'breakbeat'],
    tempo: 132,
    perc: "stack [sound \"bd:4 cp\" # legato 1.1, fast 2 $ sound \"hh27*8?\" # gain 0.8]",
    bass: "n (run 8) # s \"sawbass\" # legato 0.8 # lpf 1200 # resonance 0.3",
    lead: "every 3 (|+ n 7) $ n \"0 2 ~ 3\" # s \"keys\" # up 0.1",
    tags: ['ukg', 'break'],
  },
  {
    keywords: ['dembow', 'reggaeton'],
    tempo: 96,
    perc: "sound \"bd ~ bd sd\" |+| sound \"conga:4*2?\"",
    bass: "slow 2 $ n \"0 ~ 3 2\" # s \"sawbass\" # lpf 900",
    lead: "slow 3 $ n \"0 5 7 3\" # s \"plucks\" # room 0.2",
    tags: ['dembow'],
  },
  {
    keywords: ['techno', 'hypnotic', 'club', 'warehouse'],
    tempo: 134,
    perc: "stack [sound \"bd(4,4)\", fast 2 $ sound \"cp*4?\", fast 4 $ sound \"hh27*8\" # gain 0.85]",
    bass: "n (run 4) # s \"tb303\" # lpf 1100 # resonance 0.35",
    lead: "slow 4 $ n \"0 1 0 3\" # s \"supersaw\" # cutoff 1400 # resonance 0.25",
    fx: "# room 0.3 # size 0.4",
    tags: ['techno'],
  },
  {
    keywords: ['afro', 'afrobeats', 'afrobeat'],
    tempo: 108,
    perc: "sound \"bd ~ bd sd\" |+| sound \"perc:3 perc:4?\"",
    bass: "n \"0 ~ 2 3\" # s \"sawbass\" # lpf 1000",
    lead: "slow 3 $ n \"0 2 4 7\" # s \"marimba\" # room 0.25",
    tags: ['afro'],
  },
];

const DEFAULT_RULE: Rule = {
  keywords: [],
  tempo: 118,
  perc: "stack [sound \"bd sd:2\", fast 2 $ sound \"hh27*8?\" # gain 0.75]",
  bass: "n \"0 ~ 3 5\" # s \"sawbass\" # lpf 1000",
  lead: "slow 2 $ n \"0 2 4 7\" # s \"pluck\" # room 0.2",
  tags: ['default'],
};

const includesAny = (text: string, keywords: string[]) =>
  keywords.some((k) => text.includes(k));

export function generateTidalCode(prompt: string, mood?: string): GeneratedTidal {
  const text = `${prompt} ${mood ?? ''}`.toLowerCase();

  const bpmMatch = text.match(/(\d{2,3})\s*bpm/);
  const explicitTempo = bpmMatch ? Number(bpmMatch[1]) : undefined;

  const rule =
    RULES.find((r) => includesAny(text, r.keywords)) ?? DEFAULT_RULE;
  const tempo = explicitTempo ?? rule.tempo;

  const scale = includesAny(text, ['aeolian', 'menor', 'minor'])
    ? 'aeolian'
    : includesAny(text, ['mixolydian', 'mixolidia'])
      ? 'mixolydian'
      : includesAny(text, ['dorian', 'doria'])
        ? 'dorian'
        : 'minor';

  const leads = rule.lead.replace('scale "aeolian"', `scale "${scale}"`);
  const cps = Number((tempo / 120).toFixed(3));

  const code = [
    `setcps ${cps}`,
    `d1 $ ${rule.perc} ${rule.fx ?? ''}`,
    `d2 $ ${rule.bass}`,
    `d3 $ ${leads}`,
  ]
    .map((line) => line.trim())
    .join('\n');

  const tags = [...rule.tags];
  if (!tags.includes(scale)) tags.push(scale);
  if (explicitTempo && !tags.includes('custom-bpm')) tags.push('custom-bpm');

  return { code, tempo, tags };
}
