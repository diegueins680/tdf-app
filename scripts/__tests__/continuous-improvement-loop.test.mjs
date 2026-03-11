import test from 'node:test';
import assert from 'node:assert/strict';
import {
  expandTemplate,
  parseGitHubRemote,
  verifyImprovementLoopModel,
} from '../lib/continuous-improvement-loop.mjs';
import { auditUiSource } from '../lib/ui-static-audit.mjs';

test('expandTemplate substitutes known placeholders and leaves unknown ones intact', () => {
  const rendered = expandTemplate('idea={idea_file} missing={unknown}', {
    idea_file: '/tmp/idea.md',
  });

  assert.equal(rendered, 'idea=/tmp/idea.md missing={unknown}');
});

test('parseGitHubRemote supports ssh and https remotes', () => {
  assert.deepEqual(parseGitHubRemote('git@github.com:diegueins680/tdf-app.git'), {
    owner: 'diegueins680',
    repo: 'tdf-app',
  });

  assert.deepEqual(parseGitHubRemote('https://github.com/diegueins680/tdf-app.git'), {
    owner: 'diegueins680',
    repo: 'tdf-app',
  });
});

test('verifyImprovementLoopModel passes its own safety checks', () => {
  const report = verifyImprovementLoopModel();

  assert.equal(report.ok, true);
  assert.equal(report.findings.length, 0);
  assert.ok(report.reachableStates.includes('pollCi'));
});

test('auditUiSource flags unlabeled icon buttons and unlabeled images', () => {
  const source = `
    <IconButton onClick={toggleSidebar}>
      <MenuIcon />
    </IconButton>
    <TextField value={value} onChange={setValue} />
    <img src="/hero.png" />
    <IconButton aria-label="Open inbox">
      <MailIcon />
    </IconButton>
  `;

  const findings = auditUiSource(source, 'Example.tsx');
  assert.equal(findings.length, 3);
  assert.deepEqual(
    findings.map((finding) => finding.rule),
    ['icon-button-label', 'text-field-label', 'image-alt'],
  );
});

test('auditUiSource does not treat arrow functions as the end of a labeled tag', () => {
  const source = `
    <IconButton
      size="small"
      onClick={() => deleteItem(item.id)}
      aria-label={\`Eliminar nota: \${item.text}\`}
    >
      <DeleteIcon fontSize="small" />
    </IconButton>
  `;

  const findings = auditUiSource(source, 'Example.tsx');
  assert.equal(findings.length, 0);
});
