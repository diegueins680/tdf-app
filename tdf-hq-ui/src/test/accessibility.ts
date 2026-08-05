import axe, { type ElementContext, type RunOptions } from 'axe-core';

const JSDOM_AXE_OPTIONS: RunOptions = {
  resultTypes: ['violations'],
  rules: {
    // JSDOM has no layout/paint engine, so contrast remains covered by token
    // tests and browser-level audits rather than producing false confidence here.
    'color-contrast': { enabled: false },
  },
};

export async function expectNoSeriousAccessibilityViolations(
  context: ElementContext,
): Promise<void> {
  const results = await axe.run(context, JSDOM_AXE_OPTIONS);
  const blocking = results.violations.filter(
    ({ impact }) => impact === 'critical' || impact === 'serious',
  );

  if (blocking.length === 0) return;

  const details = blocking
    .map(({ help, id, impact, nodes }) => {
      const targets = nodes
        .slice(0, 5)
        .map(({ target }) => `  - ${target.join(' ')}`)
        .join('\n');
      return `[${impact}] ${id}: ${help}\n${targets}`;
    })
    .join('\n\n');

  throw new Error(`Serious accessibility violations detected:\n\n${details}`);
}
