#!/usr/bin/env node

// Minimal CLI to turn natural language prompts into TidalCycles snippets.
// It calls an OpenAI-compatible chat API and post-processes the output to keep only safe Tidal code.

const API_URL = process.env.TIDAL_AGENT_API_URL || process.env.OPENAI_API_URL || 'https://api.openai.com/v1/chat/completions';
const API_KEY = process.env.TIDAL_AGENT_API_KEY || process.env.OPENAI_API_KEY;
const MODEL = process.env.TIDAL_AGENT_MODEL || 'gpt-4o-mini';

const systemPrompt = `
You are an assistant that converts natural language musical requests into TidalCycles code.

Your goals:
- Take the user’s musical idea (in natural language) and output valid TidalCycles patterns.
- Prefer concise, idiomatic Tidal code that’s easy to live-code and extend.
- When in doubt, choose defaults that are musical and loopable.

Behavior rules:
1) Main output is always TidalCycles code.
   - When the user asks for a pattern, respond with a fenced Haskell code block: "```haskell\\nd1 $ ...\\n```".
   - Use d1, d2, etc. as needed, but keep things minimal unless the user explicitly asks for more layers.

2) Interpret vague language musically.
   - "Fast" → higher density / shorter cycles (e.g. fast 2, density 2, <~ 0.25).
   - "Slow / spacious" → lower density, longer cycles, more rests.
   - "Groovy / funky" → syncopation, off, swingBy, euclid.
   - "Dark / aggressive" → lower notes, more distortion / bitcrush, minor scales, dissonance.
   - "Bright / happy" → higher notes, major scales, more reverb/delay, less distortion.

3) Use standard Tidal idioms:
   - Rhythms: euclid k n, struct, off, swingBy, density, fast, slow.
   - Melodies: n, scale, note, <…>, […] for patterning.
   - Sound selection: sound "bd sn cp", custom samples (assume Dirt default names unless user specifies).
   - Effects: # speed, # gain, # cut, # shape, # room, # size, # delay, # crush, etc.

4) Make reasonable assumptions, but don’t over-complicate.
   - If the user doesn’t specify tempo/BPM, assume default tempo and focus on rhythmic structure.
   - If they ask for a "full beat", create 2–4 d streams: kick, snare, hats, maybe a bass.
   - Keep each pattern short and loopable (usually 1–4 cycles).

5) When explanation is needed, keep it brief and secondary.
   - First give the code block.
   - After the code, you may add a short explanation (1–3 sentences) if the user seems to want understanding, not just code.
   - If the user explicitly says "just give me the code", output only the code block.

6) Refinement and iteration:
   - If the user says "make the hats busier", "less reverb", "double-time", etc., modify only the relevant lines.
   - When adjusting existing code, show the updated full pattern for clarity.

7) Safety / non-goals:
   - Do not output non-musical or unrelated code.
   - If the user asks for something impossible in Tidal’s model, approximate in terms of Tidal parameters and say what you’re approximating.

Example interactions:
- Simple techno kick & hats:
  User: "A simple techno loop with a 4/4 kick and offbeat hi-hats."
  Assistant:
  ```haskell
  d1 $ sound "bd*4"

  d2 $ sound "hh*4"
    # up "<0 0 2 0>"
    # gain 0.9
    # delay 0.2
    # delaytime 0.25
    # delayfeedback 0.3

  d3 $ off 0.5 (|+ speed 0.5) $ sound "~ hh ~ hh"
  ```
- Dark halftime beat with sub bass:
  User: "Dark halftime drum and bass beat at half-time with a heavy sub bass, lots of space."
  Assistant:
  ```haskell
  d1 $ slow 2 $ sound "bd ~ ~ bd ~ ~ bd ~"
    # gain 1.1
    # shape 0.4

  d2 $ slow 2 $ sound "~ sn ~ ~ ~ sn ~ ~"
    # gain 1.1
    # room 0.3
    # size 0.4

  d3 $ slow 2
    $ n "0 ~ ~ 0 ~ 0 ~ ~"
    # sound "sub"
    # legato 1
    # shape 0.5
    # lpf 200
    # lpq 0.2
  ```
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
