#!/usr/bin/env node
/**
 * analyze-experiments.js
 *
 * Weekly experiment analysis for tdf-label-creative.
 * Reads experiment event data and produces promote/kill/iterate decisions.
 *
 * Usage: node agents/tdf-label-creative/scripts/analyze-experiments.js
 */

const fs = require('fs');
const path = require('path');

const BACKLOG_PATH = path.join(__dirname, '../backlog.md');
const METRICS_DIR = path.join(__dirname, '../../../metrics');

function loadBacklog() {
  if (!fs.existsSync(BACKLOG_PATH)) return null;
  return fs.readFileSync(BACKLOG_PATH, 'utf8');
}

function parseActiveExperiments(content) {
  // Extract experiments in "testing" status
  const sections = content.split('### ');
  const active = [];

  for (const section of sections) {
    if (section.includes('Status:** testing') || section.includes('Status: testing')) {
      const id = section.match(/^(IDEA-\d+):/)?.[1];
      const hypothesis = section.match(/\*\*Hypothesis:\*\* (.+)/)?.[1];
      const metrics = section.match(/\*\*Metrics:\*\* (.+)/)?.[1];
      if (id) active.push({ id, hypothesis, metrics });
    }
  }

  return active;
}

function loadExperimentData(experimentId, metricsDir = METRICS_DIR) {
  // In real implementation, fetch from backend or analytics API
  // For now, look for local metrics files
  const dataPath = path.join(metricsDir, `${experimentId}.json`);
  if (!fs.existsSync(dataPath)) return null;
  try {
    return JSON.parse(fs.readFileSync(dataPath, 'utf8'));
  } catch (error) {
    if (error instanceof SyntaxError) return null;
    throw error;
  }
}

function analyzeExperiment(experiment, data) {
  if (!data) {
    return {
      decision: 'WAIT',
      reason: 'No data available yet. Ensure events are being tracked.',
      action: 'Verify instrumentation is working. Check /api/v1/events endpoint.'
    };
  }

  const { variantA, variantB, duration } = data;
  const minSampleSize = 100;
  const minDuration = 7; // days

  if (duration < minDuration) {
    return {
      decision: 'WAIT',
      reason: `Only ${duration} days of data. Need ${minDuration} days minimum.`,
      action: `Continue running. Re-analyze on day ${minDuration}.`
    };
  }

  if (variantA.users < minSampleSize || variantB.users < minSampleSize) {
    return {
      decision: 'WAIT',
      reason: `Insufficient sample size. A: ${variantA.users}, B: ${variantB.users}. Need ${minSampleSize} per variant.`,
      action: 'Continue running or increase traffic allocation.'
    };
  }

  // Calculate relative lift
  const lift = ((variantB.conversionRate - variantA.conversionRate) / variantA.conversionRate * 100).toFixed(1);
  const isSignificant = Math.abs(lift) > 10; // 10% threshold

  if (lift > 10) {
    return {
      decision: 'PROMOTE',
      reason: `Variant B shows +${lift}% lift in ${data.primaryMetric} (${variantB.conversionRate}% vs ${variantA.conversionRate}%)`,
      action: 'Ship variant B to 100% of users. Update codebase. Schedule follow-up experiment to optimize further.'
    };
  } else if (lift < -10) {
    return {
      decision: 'KILL',
      reason: `Variant B shows ${lift}% decline in ${data.primaryMetric} (${variantB.conversionRate}% vs ${variantA.conversionRate}%)`,
      action: 'Kill variant B. Keep control. Document learning in backlog.'
    };
  } else {
    return {
      decision: 'ITERATE',
      reason: `Inconclusive result: ${lift}% lift (within -10% to +10% band).`,
      action: 'Design follow-up experiment with larger change or different approach.'
    };
  }
}

function generateReport(experiments) {
  const timestamp = new Date().toISOString();
  let report = `# Experiment Analysis Report — ${timestamp}\n\n`;

  if (experiments.length === 0) {
    report += 'No active experiments found in testing status.\n\n';
    report += '## Next Steps\n';
    report += '1. Pick top idea from backlog\n';
    report += '2. Build MVP experiment\n';
    report += '3. Ship to preview channel\n';
    report += '4. Re-run this analysis after 7 days\n';
    return report;
  }

  for (const exp of experiments) {
    const data = loadExperimentData(exp.id);
    const analysis = analyzeExperiment(exp, data);

    report += `## ${exp.id}\n`;
    report += `**Hypothesis:** ${exp.hypothesis}\n`;
    report += `**Target Metrics:** ${exp.metrics}\n\n`;
    report += `**Decision:** ${analysis.decision}\n`;
    report += `**Reason:** ${analysis.reason}\n`;
    report += `**Action:** ${analysis.action}\n\n`;
    report += `---\n\n`;
  }

  return report;
}

function main() {
  console.log('🎨 tdf-label-creative: Experiment Analyzer\n');

  const backlog = loadBacklog();
  if (!backlog) {
    console.log('❌ No backlog found. Run generate-ideas.js first.');
    process.exit(1);
  }

  const active = parseActiveExperiments(backlog);
  console.log(`Found ${active.length} active experiment(s)\n`);

  const report = generateReport(active);
  console.log(report);

  // Save report
  const reportPath = path.join(REPORTS_DIR || '.', `experiment-analysis-${new Date().toISOString().split('T')[0]}.md`);
  fs.mkdirSync(path.dirname(reportPath), { recursive: true });
  fs.writeFileSync(reportPath, report);
  console.log(`Report saved to ${reportPath}`);
}

if (require.main === module) {
  main();
}

module.exports = {
  generateReport,
  loadExperimentData,
};
