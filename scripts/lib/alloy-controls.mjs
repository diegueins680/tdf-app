// Mutate only a named fact/predicate in a temporary copy, preserving assertions
// and their original scopes. These controls establish sensitivity, not refinement.
export const alloyControls = [
  { model: 'EventStructure.als', controls: [
    ['accountable', 'ExactlyOneAccountable', 'fact RaciIsCompleteAndScoped',
      'one {a: RaciAssignment | a.task = t and a.role = Accountable}', 'no none'],
    ['responsible', 'RequiredResponsibleIsPresent', 'fact RaciIsCompleteAndScoped',
      'some {a: RaciAssignment | a.task = t and a.role = Responsible}', 'no none'],
    ['cycle', 'DependenciesAreAcyclic', 'fact TaskGraphIsEventLocalAndAcyclic',
      'no t: Task | t in t.^dependsOn', 'no none'],
    ['consent', 'ConfirmedContractUsesOneAcceptedVersion', 'fact ConfirmedContractsHaveMatchingConsent',
      'one a: Acceptance | a.party = p and a.version = c.current', 'no none'],
    ['exclusive', 'ExclusiveConflictHasAuthorizedOverride', 'fact ExclusiveConfirmedBookingsDoNotSilentlyConflict',
      'some left.override + right.override', 'no none'],
    ['scope-expansion', 'InvitationCannotExpandScopes', 'fact InvitationConversionIsAttenuating',
      'i.convertedGrant.scopes in i.invitedScopes', 'no none'],
    ['private-read', 'RestrictedReadRequiresEventRelationship', 'pred canRead',
      '(object.visibility = Private and p in object.event.owners)', '(object.visibility = Private)'],
    ['orphan-raci', 'RaciAssignmentsAreNotOrphaned', 'fact RaciIsCompleteAndScoped',
      '{a: RaciAssignment | a.task = t}.party in eventMembers[t.event]', 'no none'],
  ] },
  { model: 'TaskReadStructure.als', controls: [
    ['cross-event', 'TargetIsEventLocal', 'pred matches', 't.event = e', 'no none'],
    ['widen-task', 'ExactGrantDoesNotWiden', 'pred matches', 'no g.task or g.task = t', 'no none'],
    ['revoked', 'NoAmbientAuthority', 'pred matches', 'g in Current.effective', 'no none'],
    ['foreign-actor', 'NoAmbientAuthority', 'pred matches', 'g.grantee = p', 'no none'],
    ['wrong-scope', 'NoAmbientAuthority', 'pred matches', 'g.scope in TaskRead + TaskManage', 'no none'],
    ['non-manager', 'CandidateRequiresCurrentManager', 'pred editorCandidate', 'canManage[actor, e, t] and ', ''],
    ['foreign-candidate', 'CandidateIsTaskScoped', 'pred editorCandidate', ' and canRead[candidate, e, t]', ''],
  ] },
];

// Mask comments without moving offsets. Reject strings: the admitted model
// subset uses none, and braces/keywords in strings need a real lexer.
function codeMask(source) {
  const masked = source.replace(/\/\*[\s\S]*?\*\/|\/\/[^\n]*|--[^\n]*/g,
    text => text.replace(/[^\n]/g, ' '));
  if (masked.includes('"') || masked.includes('/*')) throw new Error('Unsupported Alloy source syntax');
  return masked;
}

export function alloyCommands(source) {
  const code = codeMask(source);
  const starts = [...code.matchAll(/^[ \t]*(run|check)\b[^\n]*/gm)];
  const commands = starts.map((match, index) => {
    const parsed = /^\s*(run|check)\s+([A-Za-z][A-Za-z0-9_]*)\s+for\s+\d+\b/.exec(match[0]);
    if (!parsed) throw new Error('Unsupported Alloy command declaration');
    const declaration = code.slice(match.index, starts[index + 1]?.index ?? code.length).trim();
    // These models keep commands in a terminal block. Reject extensions rather
    // than silently misparse a declaration or changed scope after a command.
    if (!/^(run|check)\s+\w+\s+for\s+\d+(?:\s+but\s+(?:exactly\s+)?\d+\s+\w+(?:\s*,\s*(?:exactly\s+)?\d+\s+\w+)*)?$/.test(declaration)) {
      throw new Error('Unsupported Alloy command scope');
    }
    return { type: parsed[1], name: parsed[2], source: declaration,
      overall: Number(/\bfor\s+(\d+)/.exec(declaration)[1]),
      bitwidth: Number(/\b(\d+)\s+Int\b/.exec(declaration)?.[1] ?? 4) };
  });
  // Fail closed on inline/labelled commands rather than silently skip them.
  if ((code.match(/\b(?:run|check)\b/g) || []).length !== commands.length
      || new Set(commands.map(c => c.name)).size !== commands.length || !commands.length) {
    throw new Error('Ambiguous Alloy command inventory');
  }
  return commands;
}

export function mutateAlloy(source, control) {
  const [, , declaration, before, after] = control;
  if (!/^(fact|pred) [A-Za-z][A-Za-z0-9_]*$/.test(declaration) || !before || before === after) {
    throw new Error('Mutation must change a named fact or predicate');
  }
  const code = codeMask(source);
  const matches = [...code.matchAll(new RegExp(`^${declaration}\\b[^\\{]*\\{`, 'gm'))];
  if (matches.length !== 1) throw new Error(`Missing or ambiguous declaration: ${declaration}`);
  const start = matches[0].index + matches[0][0].length;
  let end = start, depth = 1;
  for (; end < code.length && depth; end++) {
    if (code[end] === '{') depth++;
    if (code[end] === '}') depth--;
  }
  if (depth) throw new Error('Unclosed Alloy declaration');
  const body = source.slice(start, end - 1);
  const parts = body.split(before);
  if (parts.length !== 2 || !code.slice(start, end - 1).includes(before)) {
    throw new Error(`Mutation target drift: ${declaration}`);
  }
  return source.slice(0, start) + parts.join(after) + source.slice(end - 1);
}

export function validateAlloyCoverage(source, controls) {
  const commands = alloyCommands(source);
  const assertions = new Set(commands.filter(c => c.type === 'check').map(c => c.name));
  const declared = [...codeMask(source).matchAll(/\bassert\s+(\w+)\s*\{/g)].map(m => m[1]);
  if (!commands.some(c => c.type === 'run') || !assertions.size
      || declared.length !== assertions.size || declared.some(name => !assertions.has(name))
      || new Set(controls.map(c => c[0])).size !== controls.length
      || [...assertions].some(name => !controls.some(c => c[1] === name))
      || controls.some(c => !assertions.has(c[1]))) throw new Error('Alloy control coverage drift');
  for (const control of controls) mutateAlloy(source, control);
  return commands;
}

export function admitAlloyResult({ status, signal, error, log, receipt, command, expected }) {
  if (error || signal || status !== 0) throw new Error('Alloy execution failed');
  // Alloy's SAT progress counter uses terminal backspaces even through a pipe.
  let rendered = log;
  while (/[^\n\x08]\x08/.test(rendered)) rendered = rendered.replace(/[^\n\x08]\x08/g, '');
  const results = [...rendered.matchAll(/^\d+\.\s+(run|check)\s+(\w+)\s+(0|1\/1)\s+(SAT|UNSAT)\s*$/gm)];
  const keys = Object.keys(receipt?.commands || {});
  const recorded = receipt?.commands?.[command.name];
  if (results.length !== 1 || results[0][1] !== command.type || results[0][2] !== command.name
      || results[0][4] !== expected || results[0][3] !== (expected === 'SAT' ? '1/1' : '0')
      || keys.length !== 1 || keys[0] !== command.name
      || recorded?.type !== command.type || recorded?.name !== command.name || receipt.solver !== 'sat4j'
      || typeof recorded.source !== 'string' || typeof command.source !== 'string'
      || recorded.source.replace(/\s+/g, ' ').trim() !== command.source.replace(/\s+/g, ' ').trim()
      || recorded.overall !== command.overall || recorded.bitwidth !== command.bitwidth) {
    throw new Error('Alloy result is missing, unexpected or incomplete');
  }
  const solutions = recorded.solution || [];
  if (!Array.isArray(solutions) || (expected === 'SAT' ? solutions.length !== 1 : solutions.length !== 0)) {
    throw new Error('Alloy solution evidence disagrees with the result');
  }
  if (expected === 'SAT' && (!Array.isArray(solutions[0]?.instances) || !solutions[0].instances.length
      || solutions[0].instances.some(instance => !instance?.values || typeof instance.values !== 'object'
        || Array.isArray(instance.values) || !Object.keys(instance.values).length))) {
    throw new Error('Alloy SAT result has no retained witness instance');
  }
}
