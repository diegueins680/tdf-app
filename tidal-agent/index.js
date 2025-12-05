#!/usr/bin/env node

// Minimal CLI to turn natural language prompts into TidalCycles snippets.
// It calls an OpenAI-compatible chat API and post-processes the output to keep only safe Tidal code.

const API_URL = process.env.TIDAL_AGENT_API_URL || process.env.OPENAI_API_URL || 'https://api.openai.com/v1/chat/completions';
const API_KEY = process.env.TIDAL_AGENT_API_KEY || process.env.OPENAI_API_KEY;
const MODEL = process.env.TIDAL_AGENT_MODEL || 'gpt-4o-mini';

const systemPrompt = `
You are a TidalCycles code generator.
- Reply ONLY with TidalCycles code, no prose.
- Keep patterns short, loopable, and safe to evaluate.
- Use d1/d2/d3/d4, stack, sound, n, note, degradeBy, slow, fast, every, sometimesBy, cps/bps.
- Avoid file I/O, shell commands, or anything outside Tidal.
- Prefer concise, percussive examples; cap length to a few lines.
`.trim();

const allowedLine = (line) => {
  const trimmed = line.trim();
  if (!trimmed) return false;
  const safePrefixes = ['d1', 'd2', 'd3', 'd4', 'hush', 'bps', 'cps', 'setcps', 'once', 'solo', 'unsolo', 'all', 'xfade', 'xfadeIn'];
  return safePrefixes.some((p) => trimmed.startsWith(p));
};

const extractCode = (text) => {
  // Prefer fenced code
  const fenceMatch = text.match(/```(?:tidal)?\\s*([\\s\\S]*?)```/i);
  const body = fenceMatch ? fenceMatch[1] : text;
  const lines = body
    .split('\\n')
    .map((l) => l.replace(/[\\r\\t]/g, '').trimEnd())
    .filter((l) => !l.startsWith('--') && !l.startsWith('//'));
  const filtered = lines.filter(allowedLine);
  if (filtered.length === 0) return null;
  // Trim to a reasonable number of lines
  return filtered.slice(0, 12).join('\\n');
};

async function callModel(userPrompt) {
  if (!API_KEY) {
    throw new Error('Set TIDAL_AGENT_API_KEY or OPENAI_API_KEY to call the model.');
  }
  const body = {
    model: MODEL,
    messages: [
      { role: 'system', content: systemPrompt },
      { role: 'user', content: userPrompt },
    ],
    temperature: 0.6,
    max_tokens: 300,
  };
  const resp = await fetch(API_URL, {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
      Authorization: `Bearer ${API_KEY}`,
    },
    body: JSON.stringify(body),
  });
  if (!resp.ok) {
    const errText = await resp.text();
    throw new Error(`API error ${resp.status}: ${errText}`);
  }
  const json = await resp.json();
  const content = json?.choices?.[0]?.message?.content;
  if (!content || typeof content !== 'string') {
    throw new Error('No content returned by model.');
  }
  return content;
}

async function readStdin() {
  return new Promise((resolve) => {
    let data = '';
    process.stdin.setEncoding('utf8');
    process.stdin.on('data', (chunk) => {
      data += chunk;
    });
    process.stdin.on('end', () => resolve(data.trim()));
  });
}

async function main() {
  const args = process.argv.slice(2);
  const prompt = args.length > 0 ? args.join(' ') : await readStdin();
  if (!prompt) {
    console.error('Usage: tidal-agent \"describe the groove\"');
    console.error('Or pipe a prompt: echo \"dub techno groove\" | tidal-agent');
    process.exit(1);
  }
  try {
    const raw = await callModel(prompt);
    const code = extractCode(raw);
    if (!code) {
      console.error('Model responded without usable Tidal code.');
      process.exit(1);
    }
    process.stdout.write(`${code}\\n`);
  } catch (err) {
    console.error(`Error: ${err instanceof Error ? err.message : String(err)}`);
    process.exit(1);
  }
}

main();
