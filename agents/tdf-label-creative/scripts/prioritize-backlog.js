#!/usr/bin/env node
/**
 * prioritize-backlog.js
 *
 * Re-sorts the creative backlog by ICE score and readiness.
 * Outputs top 3 experiments to run next.
 *
 * Usage: node agents/tdf-label-creative/scripts/prioritize-backlog.js
 */

const fs = require('fs');
const path = require('path');

const BACKLOG_PATH = path.join(__dirname, '../backlog.md');

function loadBacklog() {
  if (!fs.existsSync(BACKLOG_PATH)) return [];
  const content = fs.readFileSync(BACKLOG_PATH, 'utf8');

  // Parse each IDEA section
  const ideas = [];
  const sections = content.split('### ');

  for (const section of sections) {
    const id = section.match(/^(IDEA-\d+):/)?.[1];
    if (!id) continue;

    const title = section.match(/^IDEA-\d+: (.+)/)?.[1]?.split('\n')[0];
    const iceMatch = section.match(/ICE Score:\*\* Impact (\d+) \/ Confidence (\d+) \/ Ease (\d+)/);
    const status = section.match(/Status:\*\* (\w+)/)?.[1] || 'unknown';

    const ice = iceMatch ? {
      impact: parseInt(iceMatch[1]),
      confidence: parseInt(iceMatch[2]),
      ease: parseInt(iceMatch[3]),
      total: parseInt(iceMatch[1]) * parseInt(iceMatch[2]) * parseInt(iceMatch[3])
    } : null;

    ideas.push({ id, title, ice, status, raw: section });
  }

  return ideas;
}

function prioritize(ideas) {
  // Filter to scored, non-shipped/non-killed ideas
  const candidates = ideas.filter(i =>
    i.ice &&
    i.status !== 'shipped' &&
    i.status !== 'killed'
  );

  // Sort by ICE total descending
  return candidates.sort((a, b) => b.ice.total - a.ice.total);
}

function main() {
  console.log('🎨 tdf-label-creative: Backlog Prioritizer\n');

  const ideas = loadBacklog();
  console.log(`Total ideas in backlog: ${ideas.length}`);

  const prioritized = prioritize(ideas);
  console.log(`Scored candidates: ${prioritized.length}\n`);

  console.log('=== TOP 3 EXPERIMENTS TO RUN ===\n');
  prioritized.slice(0, 3).forEach((idea, i) => {
    console.log(`${i + 1}. ${idea.id}: ${idea.title}`);
    console.log(`   ICE: ${idea.ice.impact} × ${idea.ice.confidence} × ${idea.ice.ease} = ${idea.ice.total}`);
    console.log(`   Status: ${idea.status}`);
    console.log('');
  });

  if (prioritized.length === 0) {
    console.log('No scored candidates found.');
    console.log('Run generate-ideas.js, then score new ideas with ICE framework.');
  }

  console.log('\n=== NEXT STEPS ===');
  console.log('1. Review top 3 with CTO for technical feasibility');
  console.log('2. Pick 1-2 to build this sprint');
  console.log('3. Move to "Active Experiments" section in backlog');
  console.log('4. Set up event tracking before shipping');
}

main();
