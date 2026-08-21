import { mkdir, readFile, writeFile } from 'node:fs/promises';
import path from 'node:path';
import { fileURLToPath } from 'node:url';

const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const personaPath = path.join(root, 'test/personas/personas.json');
const blueprintPath = path.join(root, 'test/personas/scenario-blueprints.json');
const resultPath = path.join(root, 'docs/persona-testing/execution-results.json');
const registryPath = path.join(root, 'tdf-hq/assets/feature-registry.json');
const docsPath = path.join(root, 'docs/persona-testing');
const generatedScenarioPath = path.join(root, 'test/personas/scenarios.json');

const requiredPersonaFields = [
  'id', 'name', 'fictional', 'email', 'biography', 'location', 'language', 'goals',
  'motivations', 'roles', 'permissionsExpected', 'preexistingData', 'technicalProficiency',
  'device', 'connectivity', 'accessibility', 'budget', 'privacyTrustConcerns',
  'primaryEpics', 'expectedSuccessCriteria',
];
const requiredBlueprintFields = [
  'epic', 'feature', 'title', 'journey', 'edgeCases', 'permission', 'testType', 'testId',
];
const validStatuses = new Set([
  'verified-automated', 'direct-observation', 'specified-not-executed',
  'blocked-environment', 'blocked-external', 'simulated-hypothesis',
]);

const readJson = async (file) => JSON.parse(await readFile(file, 'utf8'));
const quoteCsv = (value) => {
  const text = value === null || value === undefined ? '' : String(value);
  return /[",\n]/.test(text) ? `"${text.replaceAll('"', '""')}"` : text;
};
const makeCsv = (headers, rows) => `${headers.join(',')}\n${rows.map((row) =>
  headers.map((header) => quoteCsv(row[header])).join(',')).join('\n')}\n`;
const list = (values) => values.length ? values.join(', ') : 'None';
const mdEscape = (value) => String(value).replaceAll('|', '\\|').replaceAll('\n', ' ');

async function loadInputs() {
  const [personas, blueprints, registry] = await Promise.all([
    readJson(personaPath), readJson(blueprintPath), readJson(registryPath),
  ]);
  let results = { runs: [], storyResults: [] };
  try { results = await readJson(resultPath); } catch (error) {
    if (error.code !== 'ENOENT') throw error;
  }
  return { personas, blueprints, registry, results };
}

export function validateProgram({ personas, blueprints, registry, results }) {
  const errors = [];
  const records = personas.personas ?? [];
  if (records.length < 20 || records.length > 30) errors.push('Persona catalog must contain 20-30 personas.');
  const personaIds = new Set();
  const emails = new Set();
  const epicIds = new Set((blueprints.epics ?? []).map((epic) => epic.id));
  const registryIds = new Set((registry.features ?? []).map((feature) => feature.id));
  const testIds = new Set();
  const storyIds = new Set();
  let storyNumber = 0;
  for (const persona of records) {
    for (const field of requiredPersonaFields) if (persona[field] === undefined) errors.push(`${persona.id ?? '<unknown>'}: missing ${field}`);
    if (persona.fictional !== true) errors.push(`${persona.id}: fictional must be true`);
    if (!persona.email?.endsWith('@persona.test')) errors.push(`${persona.id}: email must use persona.test`);
    if (personaIds.has(persona.id)) errors.push(`${persona.id}: duplicate persona id`);
    if (emails.has(persona.email)) errors.push(`${persona.id}: duplicate email`);
    personaIds.add(persona.id); emails.add(persona.email);
    const stories = blueprints.storiesByPersona?.[persona.id] ?? [];
    if (stories.length < 3 || stories.length > 5) errors.push(`${persona.id}: must have 3-5 principal journeys`);
    const storyEpics = new Set(stories.map((story) => story.epic));
    for (const epic of persona.primaryEpics ?? []) if (!storyEpics.has(epic)) errors.push(`${persona.id}: primary epic ${epic} lacks a journey`);
    for (const story of stories) {
      storyNumber += 1;
      const storyId = `ST-${String(storyNumber).padStart(3, '0')}`;
      storyIds.add(storyId);
      for (const field of requiredBlueprintFields) if (story[field] === undefined) errors.push(`${storyId}: missing ${field}`);
      if (!epicIds.has(story.epic)) errors.push(`${storyId}: unknown epic ${story.epic}`);
      // Composite/cross-epic features intentionally use stable program identifiers; repository features must
      // otherwise be recognizable from the registry or a documented program namespace.
      if (!registryIds.has(story.feature) && !/^(auth|profile|profiles|payments|refunds|finance|operations|marketplace|school|label|accessibility|localization|support|mobile|events|distribution|internships|domo|lead-to-reconciliation|public|services|tickets|admin|crm|studio|social|live-sessions|directory)\./.test(story.feature)
        && story.feature !== 'live-sessions' && story.feature !== 'lead-to-reconciliation') {
        errors.push(`${storyId}: feature ${story.feature} is not traceable to a registry/program namespace`);
      }
      if (!Array.isArray(story.edgeCases) || story.edgeCases.length < 3) errors.push(`${storyId}: at least three edge cases required`);
      if (testIds.has(story.testId)) errors.push(`${storyId}: duplicate test id ${story.testId}`);
      testIds.add(story.testId);
    }
  }
  for (const key of Object.keys(blueprints.storiesByPersona ?? {})) if (!personaIds.has(key)) errors.push(`Stories reference unknown persona ${key}`);
  const coveredEpics = new Set(Object.values(blueprints.storiesByPersona ?? {}).flat().map((story) => story.epic));
  for (const epic of epicIds) if (!coveredEpics.has(epic)) errors.push(`Epic ${epic} has no journey`);
  for (const result of results.storyResults ?? []) {
    if (!testIds.has(result.testId)) errors.push(`Execution references unknown test ${result.testId}`);
    if (!validStatuses.has(result.status)) errors.push(`${result.testId}: invalid execution status ${result.status}`);
    if (result.status === 'verified-automated' && (!result.command || !result.evidence)) {
      errors.push(`${result.testId}: verified automated result requires command and evidence`);
    }
  }
  if (errors.length) throw new Error(`Persona test program validation failed:\n- ${errors.join('\n- ')}`);
  return { personaCount: records.length, storyCount: storyNumber, epicCount: epicIds.size };
}

function networkProfile(persona) {
  const value = persona.connectivity.toLowerCase();
  if (value.includes('2g')) return { name: persona.connectivity, latencyMs: 900, downloadKbps: 250, offlineInterruptions: true };
  if (value.includes('3g') || value.includes('slow') || value.includes('dead zone') || value.includes('intermittent')) {
    return { name: persona.connectivity, latencyMs: 600, downloadKbps: 750, offlineInterruptions: true };
  }
  return { name: persona.connectivity, latencyMs: 40, downloadKbps: 10000, offlineInterruptions: false };
}

function platformsFor(persona, story) {
  const primary = persona.device.platform;
  const values = new Set([primary]);
  if (story.testType.includes('web')) values.add('responsive-web');
  if (story.testType.includes('mobile') || story.testType.includes('detox')) values.add('native-mobile');
  if (story.testType.includes('api') || story.testType.includes('backend') || story.testType.includes('integration')) values.add('backend-api');
  return [...values];
}

function buildScenarios(personas, blueprints, results) {
  const resultByTest = new Map((results.storyResults ?? []).map((result) => [result.testId, result]));
  const epicById = new Map(blueprints.epics.map((epic) => [epic.id, epic]));
  const scenarios = [];
  let sequence = 0;
  for (const persona of personas.personas) {
    for (const blueprint of blueprints.storiesByPersona[persona.id]) {
      sequence += 1;
      const id = `ST-${String(sequence).padStart(3, '0')}`;
      const execution = resultByTest.get(blueprint.testId) ?? {
        testId: blueprint.testId,
        status: 'specified-not-executed',
        findingIds: [],
        fixOrIssue: '',
        evidence: '',
        reason: 'Specified in this program; executable environment or implementation remains a coverage gap.',
      };
      scenarios.push({
        id,
        epic: epicById.get(blueprint.epic),
        persona: { id: persona.id, name: persona.name },
        title: blueprint.title,
        feature: blueprint.feature,
        goal: blueprint.journey,
        businessValue: `${blueprint.title} protects ${epicById.get(blueprint.epic).rationale.toLowerCase()}`,
        preconditions: [
          'Use a disposable local or explicitly authorized non-production database.',
          `Load deterministic fixture ${persona.id}; all records use the reserved persona.test domain and are excluded from public discovery.`,
          'Use local inboxes and fake/sandbox adapters; disable real email, messaging, social publishing, ticket delivery, payment charging, and distribution.',
        ],
        requiredRolesAndPermissions: { roles: persona.roles, expected: blueprint.permission },
        initialDatabaseState: [
          ...persona.preexistingData,
          `Scenario records carry fixture namespace ${id.toLowerCase()} and correlation id tdf-persona-${id.toLowerCase()}.`,
          'No real user, provider credential, payment instrument, media asset, or public publication exists.',
        ],
        environment: {
          platforms: platformsFor(persona, blueprint),
          browser: blueprint.testType.includes('web') ? ['Chromium', 'Firefox and WebKit for critical tagged paths'] : [],
          device: persona.device.preferred,
          language: persona.language,
          network: networkProfile(persona),
          accessibility: persona.accessibility,
        },
        acceptanceCriteria: {
          given: `Given ${persona.name} (${persona.id}) is in the isolated initial state with ${list(persona.roles)} roles`,
          when: `When they ${blueprint.journey.charAt(0).toLowerCase()}${blueprint.journey.slice(1)}`,
          then: `Then the complete lifecycle reaches one authoritative state, permissions and ownership are enforced by the backend, duplicate effects are prevented, and the UI explains outcome and recovery in ${persona.language}.`,
        },
        testSteps: [
          `Reset and load ${persona.id} plus scenario namespace ${id}; assert the initial database state.`,
          'Open the public or authenticated entry point in a fresh context; for exploratory runs, do not provide navigation hints and record time, steps, dead ends, and assistance points.',
          blueprint.journey,
          `Exercise edge cases: ${blueprint.edgeCases.join('; ')}. Include direct URL/API denial where authorization or ownership is relevant.`,
          'Inspect visible confirmation, errors, focus/status feedback, failed requests, console output, sanitized API responses, notification outbox, audit events, and authoritative database state.',
          'Continue through terminal, cancellation, archival, refund, return, or reconciliation state rather than stopping after creation; rerun the mutation once with the same idempotency key where applicable.',
          `Clean scenario namespace ${id}, deactivate its credential/token if used, and verify no public projection or real-provider outbox record remains.`,
        ],
        expectedVisibleBehavior: [
          `The interface makes “${blueprint.title}” discoverable and states current status, next action, total price or consequence where relevant.`,
          `Validation and provider errors are specific, recoverable, localized for ${persona.language}, and announced accessibly without color-only meaning.`,
          'Loading, empty, stale, interrupted, denied, cancelled, pending, and terminal states are distinguishable; resubmission does not present a second success.',
        ],
        expectedBackendStateAndSideEffects: [
          `Only records owned by or explicitly assigned to ${persona.id} change under correlation id tdf-persona-${id.toLowerCase()}.`,
          'State transitions follow the domain lifecycle; conflicts, stale versions, invalid transitions, and unauthorized direct requests are rejected server-side.',
          'Provider/browser callbacks are not treated as financial authority; verified provider events and idempotency keys produce at most one economic or inventory effect.',
        ],
        expectedNotificationsAndAuditEvents: [
          'A local/fake notification is enqueued only after the authoritative transition and only for consented fictional recipients.',
          `Audit events identify ${persona.id}, action, target, correlation id, prior/new status, and sanitized outcome without secrets or raw payment data.`,
          'Retries reuse dedupe keys and do not duplicate notifications, tickets, deliveries, reservations, payments, refunds, or audit transitions.',
        ],
        cleanupRequirements: [
          `Remove or archive all ${id} namespaced data in the disposable database.`,
          `Deactivate ${persona.email} credentials/tokens; clear local inbox, mock-provider, browser, simulator, screenshot, video, and trace artifacts according to retention policy.`,
          'Verify the public index has no fixture records and no external adapter received a real request.',
        ],
        edgeCases: blueprint.edgeCases,
        test: { type: blueprint.testType, identifier: blueprint.testId },
        execution: {
          status: execution.status,
          command: execution.command ?? '',
          findingIds: execution.findingIds ?? [],
          fixOrIssue: execution.fixOrIssue ?? '',
          evidence: execution.evidence ?? '',
          reason: execution.reason ?? '',
        },
      });
    }
  }
  return scenarios;
}

function personaCatalogMarkdown(personas) {
  const lines = [
    '# Synthetic persona catalog', '',
    '> These are deterministic fictional test models, not human participants. Reactions in this program are heuristic hypotheses only.', '',
    `Catalog: ${personas.catalogId} · Personas: ${personas.personas.length} · Reserved domain: \`${personas.isolation.emailDomain}\``, '',
  ];
  for (const persona of personas.personas) {
    lines.push(`## ${persona.id} — ${persona.name}`, '', persona.biography, '',
      `- **Location / language:** ${persona.location.city}, ${persona.location.country} · ${persona.language}`,
      `- **Roles:** ${list(persona.roles)}`,
      `- **Goals:** ${list(persona.goals)}`,
      `- **Motivations:** ${list(persona.motivations)}`,
      `- **Preexisting fixture data:** ${list(persona.preexistingData)}`,
      `- **Technical profile:** ${persona.technicalProficiency}; ${persona.device.preferred} / ${persona.device.platform}; ${persona.connectivity}`,
      `- **Accessibility:** ${list(persona.accessibility)}`,
      `- **Budget:** ${persona.budget}`,
      `- **Privacy / trust:** ${list(persona.privacyTrustConcerns)}`,
      `- **Primary epics:** ${persona.primaryEpics.join(', ')}`,
      `- **Expected permissions:** ${list(persona.permissionsExpected)}`,
      `- **Success criteria:** ${list(persona.expectedSuccessCriteria)}`, '');
  }
  return `${lines.join('\n').trimEnd()}\n`;
}

function epicMarkdown(blueprints, scenarios) {
  const lines = [
    '# Risk-based epic inventory', '',
    'Priority combines user impact, affected reach, business value, security/data-integrity exposure, and practical testability. Equal priority numbers indicate intentionally shared urgency.', '',
    '| Priority | Epic | Risk | Stories | Why now |', '|---:|---|---|---:|---|',
  ];
  for (const epic of [...blueprints.epics].sort((a, b) => a.priority - b.priority || a.id.localeCompare(b.id))) {
    lines.push(`| ${epic.priority} | ${epic.id} — ${mdEscape(epic.title)} | ${epic.risk} | ${scenarios.filter((story) => story.epic.id === epic.id).length} | ${mdEscape(epic.rationale)} |`);
  }
  return `${lines.join('\n').trimEnd()}\n`;
}

function journeysMarkdown(scenarios) {
  const lines = [
    '# Detailed persona journey specifications', '',
    'Execution labels are evidence claims: `verified-automated`, `direct-observation`, `specified-not-executed`, `blocked-environment`, `blocked-external`, or `simulated-hypothesis`.', '',
  ];
  for (const scenario of scenarios) {
    lines.push(`## ${scenario.id} — ${scenario.title}`, '',
      `- **Epic / feature:** ${scenario.epic.id} — ${scenario.epic.title} · \`${scenario.feature}\``,
      `- **Persona:** ${scenario.persona.id} — ${scenario.persona.name}`,
      `- **Goal and business value:** ${scenario.goal} ${scenario.businessValue}.`,
      `- **Roles / permission:** ${list(scenario.requiredRolesAndPermissions.roles)} · ${scenario.requiredRolesAndPermissions.expected}`,
      `- **Environment:** ${scenario.environment.platforms.join(', ')} · ${scenario.environment.device} · ${scenario.environment.language} · ${scenario.environment.network.name}`,
      `- **Accessibility profile:** ${list(scenario.environment.accessibility)}`,
      `- **Test:** ${scenario.test.type} · \`${scenario.test.identifier}\` · **${scenario.execution.status}**`, '',
      '**Preconditions**', '', ...scenario.preconditions.map((value) => `1. ${value}`), '',
      '**Initial database state**', '', ...scenario.initialDatabaseState.map((value) => `- ${value}`), '',
      '**Acceptance criteria**', '',
      `- **Given:** ${scenario.acceptanceCriteria.given}`,
      `- **When:** ${scenario.acceptanceCriteria.when}`,
      `- **Then:** ${scenario.acceptanceCriteria.then}`, '',
      '**Steps**', '', ...scenario.testSteps.map((value, index) => `${index + 1}. ${value}`), '',
      '**Expected visible behavior**', '', ...scenario.expectedVisibleBehavior.map((value) => `- ${value}`), '',
      '**Expected backend state / side effects**', '', ...scenario.expectedBackendStateAndSideEffects.map((value) => `- ${value}`), '',
      '**Expected notifications / audit**', '', ...scenario.expectedNotificationsAndAuditEvents.map((value) => `- ${value}`), '',
      '**Cleanup**', '', ...scenario.cleanupRequirements.map((value) => `- ${value}`), '',
      `**Execution evidence:** ${scenario.execution.evidence || 'None yet.'} ${scenario.execution.reason || ''}`, '');
  }
  return `${lines.join('\n').trimEnd()}\n`;
}

function traceabilityCsv(personas, scenarios) {
  const personaById = new Map(personas.personas.map((persona) => [persona.id, persona]));
  const headers = ['Persona', 'Role combination', 'Epic', 'User story', 'Feature', 'Platform', 'Expected permission', 'Test type', 'Test identifier', 'Execution status', 'Finding identifiers', 'Fix or GitHub issue', 'Evidence location'];
  return makeCsv(headers, scenarios.map((story) => {
    const persona = personaById.get(story.persona.id);
    return {
      Persona: `${story.persona.id} ${story.persona.name}`,
      'Role combination': list(persona.roles), Epic: story.epic.id, 'User story': `${story.id} ${story.title}`,
      Feature: story.feature, Platform: story.environment.platforms.join(' + '),
      'Expected permission': story.requiredRolesAndPermissions.expected,
      'Test type': story.test.type, 'Test identifier': story.test.identifier,
      'Execution status': story.execution.status,
      'Finding identifiers': story.execution.findingIds.join(' + '),
      'Fix or GitHub issue': story.execution.fixOrIssue,
      'Evidence location': story.execution.evidence,
    };
  }));
}

async function generate(inputs) {
  const summary = validateProgram(inputs);
  const scenarios = buildScenarios(inputs.personas, inputs.blueprints, inputs.results);
  await mkdir(docsPath, { recursive: true });
  await Promise.all([
    writeFile(generatedScenarioPath, `${JSON.stringify({ schemaVersion: 1, generatedFrom: ['personas.json', 'scenario-blueprints.json', 'docs/persona-testing/execution-results.json'], scenarios }, null, 2)}\n`),
    writeFile(path.join(docsPath, 'persona-catalog.md'), personaCatalogMarkdown(inputs.personas)),
    writeFile(path.join(docsPath, 'epic-inventory.md'), epicMarkdown(inputs.blueprints, scenarios)),
    writeFile(path.join(docsPath, 'journey-specifications.md'), journeysMarkdown(scenarios)),
    writeFile(path.join(docsPath, 'traceability.csv'), traceabilityCsv(inputs.personas, scenarios)),
  ]);
  return summary;
}

const command = process.argv[2] ?? 'validate';
const inputs = await loadInputs();
if (command === 'generate') {
  const summary = await generate(inputs);
  console.log(`Generated persona program: ${summary.personaCount} personas, ${summary.storyCount} stories, ${summary.epicCount} epics.`);
} else if (command === 'validate') {
  const summary = validateProgram(inputs);
  console.log(`Persona program valid: ${summary.personaCount} personas, ${summary.storyCount} stories, ${summary.epicCount} epics.`);
} else {
  throw new Error(`Unknown command: ${command}`);
}
