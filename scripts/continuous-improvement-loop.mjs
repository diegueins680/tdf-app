#!/usr/bin/env node
import fs from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';
import process from 'node:process';
import { execFile, spawn } from 'node:child_process';
import { promisify, parseArgs } from 'node:util';
import { buildDefaultIdea } from './lib/discovery.mjs';
import { collectUiFindings, summarizeUiFindings } from './lib/ui-static-audit.mjs';
import {
  buildCommitContext,
  expandTemplate,
  parseIdeaMarkdown,
  parseGitHubRemote,
  summarizeCheckRuns,
  verifyImprovementLoopModel,
} from './lib/continuous-improvement-loop.mjs';

const execFileAsync = promisify(execFile);
const DEFAULTS = {
  allowDirty: false,
  maxIterations: 0,
  pollIntervalSeconds: 30,
  ciTimeoutMinutes: 30,
  pollGitHub: true,
  pushRemote: 'origin',
  stopOnNoChanges: true,
  iterationDelaySeconds: 10,
  commitMessageTemplate: '{commit_message}',
};

function sleep(milliseconds) {
  return new Promise((resolve) => {
    setTimeout(resolve, milliseconds);
  });
}

function buildContext(repoRoot, contextDir, iteration) {
  return {
    repo_root: repoRoot,
    context_dir: contextDir,
    iteration: String(iteration),
    idea_file: path.join(contextDir, 'idea.md'),
    ui_report_file: path.join(contextDir, 'ui-report.json'),
    formal_report_file: path.join(contextDir, 'formal-report.json'),
    ci_report_file: path.join(contextDir, 'ci-report.json'),
  };
}

function loopEnvironment(context) {
  return {
    ...process.env,
    CONTINUOUS_LOOP_REPO_ROOT: context.repo_root,
    CONTINUOUS_LOOP_CONTEXT_DIR: context.context_dir,
    CONTINUOUS_LOOP_ITERATION: context.iteration,
    CONTINUOUS_LOOP_IDEA_FILE: context.idea_file,
    CONTINUOUS_LOOP_UI_REPORT_FILE: context.ui_report_file,
    CONTINUOUS_LOOP_FORMAL_REPORT_FILE: context.formal_report_file,
    CONTINUOUS_LOOP_CI_REPORT_FILE: context.ci_report_file,
  };
}

async function readConfig(configPath) {
  if (!configPath) return {};
  const raw = await fs.readFile(configPath, 'utf8');
  return JSON.parse(raw);
}

async function execText(command, args, cwd) {
  try {
    return await execFileAsync(command, args, {
      cwd,
      maxBuffer: 10 * 1024 * 1024,
    });
  } catch (error) {
    const details = [error.message, error.stdout, error.stderr].filter(Boolean).join('\n');
    throw new Error(details);
  }
}

async function runShellCommand(command, repoRoot, context, options = {}) {
  const rendered = expandTemplate(command, context);
  console.log(`$ ${rendered}`);

  return new Promise((resolve, reject) => {
    const child = spawn(rendered, {
      cwd: repoRoot,
      env: loopEnvironment(context),
      shell: true,
      stdio: ['ignore', 'pipe', 'pipe'],
    });

    let stdout = '';
    let stderr = '';

    child.stdout.on('data', (chunk) => {
      const value = chunk.toString();
      stdout += value;
      process.stdout.write(value);
    });

    child.stderr.on('data', (chunk) => {
      const value = chunk.toString();
      stderr += value;
      process.stderr.write(value);
    });

    child.on('error', (error) => {
      reject(error);
    });

    child.on('close', (code) => {
      if ((code ?? 1) !== 0 && !options.allowFailure) {
        reject(new Error(`${options.stepName ?? 'Command'} failed with exit code ${code ?? 1}.`));
        return;
      }

      resolve({
        code: code ?? 1,
        stdout,
        stderr,
      });
    });
  });
}

function parseReportOutput(result, fallbackToolName) {
  const payload = [result.stdout, result.stderr].filter(Boolean).join('\n').trim();
  if (!payload) {
    return {
      ok: result.code === 0,
      findings: result.code === 0 ? [] : [{ severity: 'error', message: `${fallbackToolName} failed.` }],
      tool: fallbackToolName,
    };
  }

  try {
    const parsed = JSON.parse(payload);
    if (Array.isArray(parsed)) {
      return {
        ok: parsed.length === 0,
        findings: parsed,
        tool: fallbackToolName,
      };
    }

    if (Array.isArray(parsed.findings)) {
      return {
        ...parsed,
        ok: parsed.findings.length === 0,
        tool: parsed.tool ?? fallbackToolName,
      };
    }

    return {
      ok: result.code === 0,
      findings: [],
      raw: parsed,
      tool: fallbackToolName,
    };
  } catch {
    return {
      ok: result.code === 0,
      findings:
        result.code === 0
          ? []
          : [
              {
                severity: 'error',
                message: payload,
              },
            ],
      raw: payload,
      tool: fallbackToolName,
    };
  }
}

function logStep(label) {
  console.log(`\n== ${label} ==`);
}

function hasFindings(report) {
  return Array.isArray(report.findings) && report.findings.length > 0;
}

function findingKey(finding) {
  return [finding.rule, finding.file, finding.message, finding.snippet].join('::');
}

async function getCurrentBranch(repoRoot) {
  const { stdout } = await execText('git', ['branch', '--show-current'], repoRoot);
  return stdout.trim();
}

async function getRemoteUrl(repoRoot, remoteName) {
  const { stdout } = await execText('git', ['remote', 'get-url', remoteName], repoRoot);
  return stdout.trim();
}

async function getHeadSha(repoRoot) {
  const { stdout } = await execText('git', ['rev-parse', 'HEAD'], repoRoot);
  return stdout.trim();
}

function splitLines(stdout) {
  return stdout
    .split('\n')
    .map((value) => value.trim())
    .filter(Boolean);
}

async function listTrackedChanges(repoRoot) {
  const { stdout } = await execText('git', ['status', '--porcelain', '--untracked-files=no'], repoRoot);
  return splitLines(stdout);
}

async function listUntrackedFiles(repoRoot) {
  const { stdout } = await execText('git', ['ls-files', '--others', '--exclude-standard'], repoRoot);
  return splitLines(stdout);
}

async function hasCommitCandidateChanges(repoRoot, baselineUntracked) {
  const trackedChanges = await listTrackedChanges(repoRoot);
  if (trackedChanges.length > 0) {
    return true;
  }

  const baseline = baselineUntracked ?? new Set();
  const untrackedFiles = await listUntrackedFiles(repoRoot);
  return untrackedFiles.some((filePath) => !baseline.has(filePath));
}

async function ensureStartState(repoRoot, allowDirty) {
  const trackedChanges = await listTrackedChanges(repoRoot);
  if (trackedChanges.length > 0) {
    throw new Error(
      'Continuous improvement loop requires a clean tracked worktree. Commit or stash tracked changes first.',
    );
  }

  const initialUntracked = new Set(await listUntrackedFiles(repoRoot));
  if (!allowDirty && initialUntracked.size > 0) {
    throw new Error(
      'Continuous improvement loop found pre-existing untracked files. Pass --allow-dirty to preserve them as an ignored baseline.',
    );
  }

  return initialUntracked;
}

async function writeJson(filePath, value) {
  await fs.writeFile(filePath, `${JSON.stringify(value, null, 2)}\n`, 'utf8');
}

async function generateIdea(repoRoot, context, config) {
  if (config.ideaCommand) {
    const result = await runShellCommand(config.ideaCommand, repoRoot, context, {
      stepName: 'Idea discovery',
    });
    const markdown = [result.stdout, result.stderr].filter(Boolean).join('\n').trim();
    if (!markdown) {
      throw new Error('ideaCommand completed successfully but produced no idea text.');
    }
    await fs.writeFile(context.idea_file, `${markdown}\n`, 'utf8');
    const metadata = parseIdeaMarkdown(markdown);
    return {
      source: 'custom',
      markdown,
      ...metadata,
    };
  }

  const idea = await buildDefaultIdea(repoRoot);
  await fs.writeFile(context.idea_file, idea.markdown, 'utf8');
  const metadata = parseIdeaMarkdown(idea.markdown);
  return {
    ...metadata,
    ...idea,
    title: idea.title ?? metadata.title,
    source: idea.source ?? metadata.source,
    target: idea.target ?? metadata.target,
    reason: idea.reason ?? metadata.reason,
  };
}

async function generateUiReport(repoRoot, context, config) {
  if (config.uiAuditCommand) {
    const result = await runShellCommand(config.uiAuditCommand, repoRoot, context, {
      stepName: 'UI audit',
      allowFailure: true,
    });
    const report = parseReportOutput(result, 'custom-ui-audit');
    report.generatedAt = new Date().toISOString();
    await writeJson(context.ui_report_file, report);
    return report;
  }

  const rootDir = path.join(repoRoot, 'tdf-hq-ui', 'src');
  const allFindings = await collectUiFindings(rootDir);
  const baseline = config.uiBaselineFindingKeys ?? new Set();
  const findings = allFindings.filter((finding) => !baseline.has(findingKey(finding)));
  const report = {
    ok: findings.length === 0,
    findings,
    summary: summarizeUiFindings(findings),
    baselineSuppressedCount: allFindings.length - findings.length,
    baselineTotal: baseline.size,
    tool: 'builtin-static-ui-audit',
    generatedAt: new Date().toISOString(),
  };
  await writeJson(context.ui_report_file, report);
  return report;
}

async function generateFormalReport(repoRoot, context, config) {
  if (config.formalVerifyCommand) {
    const result = await runShellCommand(config.formalVerifyCommand, repoRoot, context, {
      stepName: 'Formal verification',
      allowFailure: true,
    });
    const report = parseReportOutput(result, 'custom-formal-verification');
    report.generatedAt = new Date().toISOString();
    await writeJson(context.formal_report_file, report);
    return report;
  }

  const report = {
    ...verifyImprovementLoopModel(),
    tool: 'builtin-loop-model-check',
    generatedAt: new Date().toISOString(),
  };
  await writeJson(context.formal_report_file, report);
  return report;
}

async function stageCommitCandidates(repoRoot, baselineUntracked) {
  await execText('git', ['add', '-u'], repoRoot);
  const untrackedFiles = await listUntrackedFiles(repoRoot);
  const newUntrackedFiles = untrackedFiles.filter((filePath) => !baselineUntracked.has(filePath));
  if (newUntrackedFiles.length > 0) {
    await execText('git', ['add', '--', ...newUntrackedFiles], repoRoot);
  }
}

async function listStagedFiles(repoRoot, baselineUntracked) {
  await stageCommitCandidates(repoRoot, baselineUntracked);
  const { stdout } = await execText('git', ['diff', '--cached', '--name-only'], repoRoot);
  return splitLines(stdout);
}

async function commitStagedChanges(repoRoot, commitMessage, stagedFiles) {
  await execText('git', ['commit', '-m', commitMessage], repoRoot);
  return {
    changed: true,
    stagedFiles,
    sha: await getHeadSha(repoRoot),
  };
}

async function pushHead(repoRoot, remoteName, branchName) {
  await execText('git', ['push', remoteName, `HEAD:${branchName}`], repoRoot);
}

async function fetchCheckRuns(repoRoot, remoteName, sha) {
  const remoteUrl = await getRemoteUrl(repoRoot, remoteName);
  const remote = parseGitHubRemote(remoteUrl);
  if (!remote) {
    throw new Error(`Unsupported Git remote for GitHub polling: ${remoteUrl}`);
  }

  const endpoint = `repos/${remote.owner}/${remote.repo}/commits/${sha}/check-runs`;
  const { stdout } = await execText(
    'gh',
    ['api', '-H', 'Accept: application/vnd.github+json', endpoint],
    repoRoot,
  );
  const payload = JSON.parse(stdout);

  return {
    owner: remote.owner,
    repo: remote.repo,
    checkRuns: payload.check_runs ?? [],
  };
}

async function waitForGreenCi(repoRoot, config, sha) {
  const deadline = Date.now() + Number(config.ciTimeoutMinutes) * 60_000;
  let sawChecks = false;

  while (true) {
    const { owner, repo, checkRuns } = await fetchCheckRuns(repoRoot, config.pushRemote, sha);
    const summary = summarizeCheckRuns(checkRuns);

    if (checkRuns.length > 0) {
      sawChecks = true;
    }

    if (summary.failed.length > 0) {
      return {
        ok: false,
        owner,
        repo,
        sha,
        summary,
        checkRuns,
      };
    }

    if ((summary.pending.length > 0 || !sawChecks) && Date.now() <= deadline) {
      console.log(
        sawChecks
          ? `Waiting for ${summary.pending.length} GitHub check(s) to finish for ${sha}...`
          : `Waiting for GitHub check runs to appear for ${sha}...`,
      );
      await sleep(Number(config.pollIntervalSeconds) * 1000);
      continue;
    }

    if (!sawChecks) {
      return {
        ok: false,
        owner,
        repo,
        sha,
        summary,
        checkRuns,
        timeout: true,
        message: 'Timed out waiting for GitHub check runs to appear.',
      };
    }

    if (summary.pending.length > 0) {
      return {
        ok: false,
        owner,
        repo,
        sha,
        summary,
        checkRuns,
        timeout: true,
        message: 'Timed out waiting for GitHub checks to finish.',
      };
    }

    return {
      ok: true,
      owner,
      repo,
      sha,
      summary,
      checkRuns,
    };
  }
}

async function finalizeIteration(repoRoot, config, context) {
  if (config.dryRun) {
    const trackedChanges = await listTrackedChanges(repoRoot);
    const newUntrackedFiles = (await listUntrackedFiles(repoRoot)).filter(
      (filePath) => !config.initialUntracked.has(filePath),
    );
    return {
      ok: true,
      dryRun: true,
      changes: [...trackedChanges, ...newUntrackedFiles.map((filePath) => `?? ${filePath}`)],
    };
  }

  while (true) {
    const stagedFiles = await listStagedFiles(repoRoot, config.initialUntracked);
    if (stagedFiles.length === 0) {
      return {
        ok: true,
        skipped: true,
        reason: 'No changes detected after this iteration.',
      };
    }

    const commitContext = buildCommitContext(context, stagedFiles);
    const commitMessage = expandTemplate(config.commitMessageTemplate, commitContext);
    const commitResult = await commitStagedChanges(repoRoot, commitMessage, stagedFiles);
    context.head_sha = commitResult.sha;
    context.last_commit_message = commitMessage;
    console.log(`Committed ${commitResult.sha} with ${commitResult.stagedFiles.length} file(s).`);

    if (!config.pushBranch) {
      config.pushBranch = await getCurrentBranch(repoRoot);
    }
    if (!config.pushBranch) {
      throw new Error('Unable to determine current branch for git push.');
    }

    console.log(`Pushing ${commitResult.sha} to ${config.pushRemote}:${config.pushBranch}`);
    await pushHead(repoRoot, config.pushRemote, config.pushBranch);

    if (!config.pollGitHub) {
      return {
        ok: true,
        pushed: true,
        sha: commitResult.sha,
      };
    }

    const ciResult = await waitForGreenCi(repoRoot, config, commitResult.sha);
    await writeJson(context.ci_report_file, ciResult);

    if (ciResult.ok) {
      return {
        ok: true,
        pushed: true,
        sha: commitResult.sha,
        ciResult,
      };
    }

    if (!config.ciRepairCommand) {
      throw new Error(
        `GitHub checks failed for ${commitResult.sha}. Configure ciRepairCommand or inspect ${context.ci_report_file}.`,
      );
    }

    logStep('CI repair');
    await runShellCommand(config.ciRepairCommand, repoRoot, context, {
      stepName: 'CI repair',
    });

    if (!(await hasCommitCandidateChanges(repoRoot, config.initialUntracked))) {
      throw new Error('ciRepairCommand completed without creating changes.');
    }
  }
}

async function runIteration(repoRoot, config, iteration) {
  const contextDir = await fs.mkdtemp(path.join(os.tmpdir(), 'continuous-improvement-loop-'));
  const context = buildContext(repoRoot, contextDir, iteration);

  logStep(`Iteration ${iteration}: idea discovery`);
  const idea = await generateIdea(repoRoot, context, config);
  context.idea_source = idea.source ?? '';
  context.idea_title = idea.title ?? '';
  context.idea_target = idea.target ?? '';
  context.idea_reason = idea.reason ?? '';

  if (!config.implementationCommand) {
    throw new Error('implementationCommand is required to run the loop unattended.');
  }

  logStep(`Iteration ${iteration}: implementation`);
  await runShellCommand(config.implementationCommand, repoRoot, context, {
    stepName: 'Implementation',
  });

  logStep(`Iteration ${iteration}: UI audit`);
  let uiReport = await generateUiReport(repoRoot, context, config);
  if (hasFindings(uiReport)) {
    if (!config.uiFixCommand) {
      throw new Error(`UI audit found issues. Configure uiFixCommand or inspect ${context.ui_report_file}.`);
    }

    logStep(`Iteration ${iteration}: UI fixes`);
    await runShellCommand(config.uiFixCommand, repoRoot, context, {
      stepName: 'UI fixes',
    });

    uiReport = await generateUiReport(repoRoot, context, config);
    if (hasFindings(uiReport)) {
      throw new Error(`UI findings remain after uiFixCommand. See ${context.ui_report_file}.`);
    }
  }

  logStep(`Iteration ${iteration}: formal verification`);
  let formalReport = await generateFormalReport(repoRoot, context, config);
  if (hasFindings(formalReport)) {
    if (!config.formalFixCommand) {
      throw new Error(
        `Formal verification found issues. Configure formalFixCommand or inspect ${context.formal_report_file}.`,
      );
    }

    logStep(`Iteration ${iteration}: formal fixes`);
    await runShellCommand(config.formalFixCommand, repoRoot, context, {
      stepName: 'Formal fixes',
    });

    formalReport = await generateFormalReport(repoRoot, context, config);
    if (hasFindings(formalReport)) {
      throw new Error(`Formal findings remain after formalFixCommand. See ${context.formal_report_file}.`);
    }
  }

  logStep(`Iteration ${iteration}: commit, push, and CI polling`);
  return finalizeIteration(repoRoot, config, context);
}

function printHelp() {
  console.log(`Usage: node scripts/continuous-improvement-loop.mjs [options]

Options:
  --config <path>           JSON config file with command hooks and git settings.
  --max-iterations <n>      0 means infinite. Default: 0.
  --allow-dirty             Allow starting from a dirty worktree.
  --dry-run                 Run the loop without committing or pushing.
  --help                    Show this help.

Placeholders available inside command hooks:
  {repo_root} {context_dir} {iteration}
  {idea_file} {ui_report_file} {formal_report_file} {ci_report_file}

Commit message templates can also use:
  {commit_message} {commit_type} {commit_summary}
  {primary_path} {files_changed} {staged_files}

Environment variables are also exported with the CONTINUOUS_LOOP_* prefix.`);
}

async function main() {
  const { values } = parseArgs({
    options: {
      config: { type: 'string' },
      'max-iterations': { type: 'string' },
      'allow-dirty': { type: 'boolean' },
      'dry-run': { type: 'boolean' },
      help: { type: 'boolean' },
    },
  });

  if (values.help) {
    printHelp();
    return;
  }

  const repoRoot = process.cwd();
  const fileConfig = await readConfig(values.config);
  const config = {
    ...DEFAULTS,
    ...fileConfig,
  };

  if (values['max-iterations'] !== undefined) {
    config.maxIterations = Number(values['max-iterations']);
  }
  if (values['allow-dirty']) {
    config.allowDirty = true;
  }
  if (values['dry-run']) {
    config.dryRun = true;
  }

  if (!Number.isFinite(Number(config.maxIterations)) || Number(config.maxIterations) < 0) {
    throw new Error('maxIterations must be a non-negative number.');
  }

  config.initialUntracked = await ensureStartState(repoRoot, config.allowDirty);
  config.uiBaselineFindingKeys = new Set();
  try {
    const baselineFindings = await collectUiFindings(path.join(repoRoot, 'tdf-hq-ui', 'src'));
    for (const finding of baselineFindings) {
      config.uiBaselineFindingKeys.add(findingKey(finding));
    }
    if (config.uiBaselineFindingKeys.size > 0) {
      console.log(`Recorded ${config.uiBaselineFindingKeys.size} baseline UI finding(s) to avoid re-fixing the existing backlog in one iteration.`);
    }
  } catch (error) {
    console.warn(`Skipping UI baseline capture: ${error.message}`);
  }

  let iteration = 1;
  while (config.maxIterations === 0 || iteration <= Number(config.maxIterations)) {
    const result = await runIteration(repoRoot, config, iteration);
    if (result.skipped && config.stopOnNoChanges) {
      console.log(result.reason);
      break;
    }
    if (Number(config.iterationDelaySeconds) > 0) {
      console.log(`Sleeping ${config.iterationDelaySeconds}s before the next iteration...`);
      await sleep(Number(config.iterationDelaySeconds) * 1000);
    }
    iteration += 1;
  }
}

main().catch((error) => {
  console.error(error.message);
  process.exit(1);
});
