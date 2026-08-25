#!/usr/bin/env node
/**
 * generate-ideas.js
 *
 * Weekly feature idea generator for tdf-label-creative.
 * Scans codebase, user feedback, and market trends to propose
 * new experiments scored with ICE framework.
 *
 * Usage: node agents/tdf-label-creative/scripts/generate-ideas.js
 */

const fs = require('fs');
const path = require('path');

const BACKLOG_PATH = path.join(__dirname, '../backlog.md');
const REPORTS_DIR = path.join(__dirname, '../../../reports');

function loadBacklog() {
  if (!fs.existsSync(BACKLOG_PATH)) return { ideas: [], lastId: 0 };
  const content = fs.readFileSync(BACKLOG_PATH, 'utf8');
  const ideas = [...content.matchAll(/### IDEA-(\d+):/g)];
  const lastId = ideas.length > 0 ? Math.max(...ideas.map(m => parseInt(m[1]))) : 0;
  return { content, lastId };
}

function generateNewIdeas(lastId) {
  // In a real implementation, this would:
  // 1. Analyze user behavior data
  // 2. Scan competitor features
  // 3. Review support tickets / feedback
  // 4. Apply creative patterns (viral loops, habit formation, social proof)
  //
  // For now, we generate structured prompts for the agent to fill in.

  const templates = [
    {
      category: 'Viral Loop',
      prompt: 'What feature would make users want to invite 3 friends within their first week?'
    },
    {
      category: 'Habit Formation',
      prompt: 'What daily or weekly ritual could we create that takes <30 seconds and feels rewarding?'
    },
    {
      category: 'Social Proof',
      prompt: 'How can we make user activity visible to others in a way that motivates participation?'
    },
    {
      category: 'Delight',
      prompt: 'What unexpected moment of joy could we add to the most common user flow?'
    },
    {
      category: 'Friction Reduction',
      prompt: 'What step in the current onboarding or upload flow causes the most drop-off?'
    }
  ];

  const nextId = lastId + 1;
  const ideas = templates.map((t, i) => ({
    id: `IDEA-${String(nextId + i).padStart(3, '0')}`,
    category: t.category,
    prompt: t.prompt,
    status: 'generated'
  }));

  return ideas;
}

function appendToBacklog(newIdeas) {
  const timestamp = new Date().toISOString();
  const section = `\n\n## Generated ${timestamp}\n\n` + newIdeas.map(idea =>
    `### ${idea.id}: [${idea.category}]\n` +
    `- **Prompt:** ${idea.prompt}\n` +
    `- **ICE Score:** Impact _ / Confidence _ / Ease _ = ___\n` +
    `- **Hypothesis:** _\n` +
    `- **Experiment:** _\n` +
    `- **Metrics:** _\n` +
    `- **Effort:** _ days\n` +
    `- **Status:** generated\n`
  ).join('\n');

  fs.appendFileSync(BACKLOG_PATH, section);
  console.log(`Generated ${newIdeas.length} new ideas. Backlog updated.`);
}

function main() {
  console.log('🎨 tdf-label-creative: Feature Idea Generator\n');

  const { lastId } = loadBacklog();
  const newIdeas = generateNewIdeas(lastId);

  console.log('New idea prompts:');
  newIdeas.forEach(idea => {
    console.log(`\n${idea.id} [${idea.category}]`);
    console.log(`  → ${idea.prompt}`);
  });

  appendToBacklog(newIdeas);

  console.log('\n✅ Next: Review and score these ideas with ICE framework.');
  console.log('   Then move top 3 to "Active Experiments" and assign to sprints.');
}

main();
