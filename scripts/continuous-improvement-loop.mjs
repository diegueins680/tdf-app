#!/usr/bin/env node
import fs from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';
import process from 'node:process';
import { execFile, execFileSync, spawn } from 'node:child_process';
import { fileURLToPath, pathToFileURL } from 'node:url';
import { promisify, parseArgs } from 'node:util';
import { discoverImprovementIdea } from './lib/discovery.mjs';
import { collectUiFindings, summarizeUiFindings } from './lib/ui-static-audit.mjs';
import { collectLogicalFindings, summarizeLogicalFindings } from './lib/logical-correctness-audit.mjs';
import { collectFailingFormalFindings, collectFormalFindings, summarizeFormalFindings } from './lib/formal-methods-audit.mjs';
import { collectUxFindings, summarizeUxFindings } from './lib/ux-quality-audit.mjs';
import {
  buildCommitContext,
  expandTemplate,
  htmlToText,
  parseIdeaMarkdown,
  parseGitHubRemote,
  summarizeCheckRuns,
  summarizeWorkflowRuns,
  verifyImprovementLoopModel,
} from './lib/continuous-improvement-loop.mjs';

const execFileAsync = promisify(execFile);
const GITHUB_LOG_LINE_LIMIT = 200;
const GITHUB_LOG_CHAR_LIMIT = 24_000;
const TOOL_ROOT = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const STEP_KILL_GRACE_MS = 10_000;
const ALLOW_PUSH_TO_MAIN_ENV = 'CONTINUOUS_LOOP_ALLOW_PUSH_TO_MAIN';

class GitHubHttpError extends Error {
  constructor(status, statusText, payload) {
    super(`GitHub API request failed (${status} ${statusText}): ${payload}`);
    this.name = 'GitHubHttpError';
    this.status = status;
    this.statusText = statusText;
    this.payload = payload;
  }
}
const DEFAULTS = {
  allowDirty: false,
  maxIterations: 0,
  pollIntervalSeconds: 30,
  ciTimeoutMinutes: 30,
  pollGitHub: true,
  pushRemote: 'origin',
  baseBranch: 'main',
  branchPrefix: 'continuous-improvement-loop/',
  openPullRequest: true,
  draftPullRequest: true,
  maxOpenPullRequests: 3,
  prBacklogDelaySeconds: 900,
  requireLinkedWorktree: true,
  stepTimeoutMinutes: 180,
  maxCiRepairAttempts: 3,
  maxConsecutiveFailures: 5,
  failureDelaySeconds: 300,
  shutdownSimulatorsBeforeImplementation: false,
  stopOnNoChanges: true,
  iterationDelaySeconds: 10,
  commitMessageTemplate: '{commit_message}',
};

let stepTimeoutMs = DEFAULTS.stepTimeoutMinutes * 60_000;
let activeStepChild = null;

function killStepGroup(child, signal) {
  if (!child?.pid) return;
  try {
    // Steps run as their own process group so nested workers die with them.
    process.kill(-child.pid, signal);
  } catch {
    try {
      child.kill(signal);
    } catch {
      // already gone
    }
  }
}

const SUPERVISOR_STATUS_FILE = process.env.CONTINUOUS_LOOP_SUPERVISOR_STATUS_FILE ?? '';

function sleep(milliseconds) {
  return new Promise((resolve) => {
    setTimeout(resolve, milliseconds);
  });
}

async function emitLoopStatus(patch = {}) {
  if (!SUPERVISOR_STATUS_FILE) return;

  try {
    let current = {};
    try {
      const raw = await fs.readFile(SUPERVISOR_STATUS_FILE, 'utf8');
      current = JSON.parse(raw);
    } catch {
      current = {};
    }

    const next = {
      ...current,
      ...patch,
      runnerPid: process.pid,
      runnerHeartbeat: new Date().toISOString(),
    };

    const tmpFile = `${SUPERVISOR_STATUS_FILE}.runner-${process.pid}.tmp`;
    await fs.writeFile(tmpFile, `${JSON.stringify(next, null, 2)}\n`, 'utf8');
    await fs.rename(tmpFile, SUPERVISOR_STATUS_FILE);
  } catch {
    // Status emission is best-effort and must not break the loop.
  }
}

let cachedGitHubToken = null;

function buildSanitizedGhAuthEnv() {
  return {
    ...process.env,
    GH_TOKEN: '',
    GITHUB_TOKEN: '',
    GITHUB_PAT: '',
  };
}

function envGitHubToken() {
  return process.env.GITHUB_TOKEN ?? process.env.GH_TOKEN ?? process.env.GITHUB_PAT ?? '';
}

function envGitHubTokenLooksInvalid(statusOutput = '') {
  return /token .* is invalid/i.test(String(statusOutput));
}

function getGitHubToken() {
  if (cachedGitHubToken !== null) {
    return cachedGitHubToken;
  }

  try {
    cachedGitHubToken = execFileSync('gh', ['auth', 'token'], {
      cwd: process.cwd(),
      env: buildSanitizedGhAuthEnv(),
      encoding: 'utf8',
      stdio: ['ignore', 'pipe', 'ignore'],
    }).trim();
    if (cachedGitHubToken) {
      return cachedGitHubToken;
    }
  } catch {
    cachedGitHubToken = null;
  }

  const fallbackToken = envGitHubToken();
  if (!fallbackToken) {
    cachedGitHubToken = '';
    return cachedGitHubToken;
  }

  try {
    execFileSync('gh', ['auth', 'status'], {
      cwd: process.cwd(),
      encoding: 'utf8',
      stdio: ['ignore', 'pipe', 'pipe'],
    });
    cachedGitHubToken = fallbackToken;
    return cachedGitHubToken;
  } catch (error) {
    const statusOutput = [error.stdout, error.stderr].filter(Boolean).join('\n');
    if (envGitHubTokenLooksInvalid(statusOutput)) {
      cachedGitHubToken = '';
      return cachedGitHubToken;
    }
  }

  // Fall back to env tokens for headless contexts where gh auth is intentionally absent.
  cachedGitHubToken = fallbackToken;
  return cachedGitHubToken;
}

function getGitHubApiBase() {
  return (process.env.GITHUB_API_BASE_URL ?? 'https://api.github.com').replace(/\/$/, '');
}

function buildContext(repoRoot, contextDir, iteration) {
  return {
    repo_root: repoRoot,
    tool_root: TOOL_ROOT,
    context_dir: contextDir,
    iteration: String(iteration),
    idea_file: path.join(contextDir, 'idea.md'),
    logical_report_file: path.join(contextDir, 'logical-report.json'),
    formal_report_file: path.join(contextDir, 'formal-report.json'),
    ux_report_file: path.join(contextDir, 'ux-report.json'),
    ui_report_file: path.join(contextDir, 'ui-report.json'),
    ci_report_file: path.join(contextDir, 'ci-report.json'),
    excluded_targets_file: path.join(contextDir, 'excluded-targets.json'),
  };
}

function loopEnvironment(context) {
  // Steps must not inherit the supervisor's status file: a nested loop run (for example the
  // loop's own tests inside the verification gate) would overwrite the live status.
  const { CONTINUOUS_LOOP_SUPERVISOR_STATUS_FILE: _statusFile, ...inheritedEnv } = process.env;
  return {
    ...inheritedEnv,
    CONTINUOUS_LOOP_REPO_ROOT: context.repo_root,
    CONTINUOUS_LOOP_TOOL_ROOT: context.tool_root,
    CONTINUOUS_LOOP_CONTEXT_DIR: context.context_dir,
    CONTINUOUS_LOOP_ITERATION: context.iteration,
    CONTINUOUS_LOOP_IDEA_FILE: context.idea_file,
    CONTINUOUS_LOOP_LOGICAL_REPORT_FILE: context.logical_report_file,
    CONTINUOUS_LOOP_FORMAL_REPORT_FILE: context.formal_report_file,
    CONTINUOUS_LOOP_UX_REPORT_FILE: context.ux_report_file,
    CONTINUOUS_LOOP_UI_REPORT_FILE: context.ui_report_file,
    CONTINUOUS_LOOP_CI_REPORT_FILE: context.ci_report_file,
    CONTINUOUS_LOOP_EXCLUDED_TARGETS_FILE: context.excluded_targets_file,
  };
}

async function shutdownBootedSimulators() {
  try {
    const { stdout } = await execFileAsync('sh', ['-c', 'xcrun simctl list devices | grep Booted | grep -oE "[A-F0-9]{8}-([A-F0-9]{4}-){3}[A-F0-9]{12}" || true'], { maxBuffer: 1024 * 1024 });
    const uuids = stdout.trim().split('\n').filter(Boolean);
    if (uuids.length === 0) return;
    console.log(`Shutting down ${uuids.length} booted simulator(s) before build…`);
    for (const udid of uuids) {
      try {
        await execFileAsync('xcrun', ['simctl', 'shutdown', udid]);
        console.log(`  shutdown ${udid}`);
      } catch (e) {
        console.warn(`  failed to shutdown ${udid}: ${e.message}`);
      }
    }
  } catch (e) {
    // simctl not available or other error — ignore
  }
}

async function readConfig(configPath) {
  if (!configPath) return {};
  const raw = await fs.readFile(configPath, 'utf8');
  return JSON.parse(raw);
}

function isTruthyFlag(value) {
  return ['1', 'true', 'yes', 'on'].includes(String(value ?? '').trim().toLowerCase());
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

function resolveGitHubApiUrl(endpoint) {
  if (/^https?:\/\//.test(endpoint)) {
    return endpoint;
  }
  return `${getGitHubApiBase()}/${String(endpoint).replace(/^\//, '')}`;
}

async function githubRequest(endpoint, options = {}) {
  const headers = {
    Accept: 'application/vnd.github+json',
    'User-Agent': 'continuous-improvement-loop',
    ...(options.headers ?? {}),
  };
  const token = getGitHubToken();
  if (token) {
    headers.Authorization = `Bearer ${token}`;
  }

  const response = await fetch(resolveGitHubApiUrl(endpoint), {
    method: 'GET',
    redirect: 'follow',
    ...options,
    headers,
  });

  if (!response.ok) {
    const payload = await response.text();
    throw new GitHubHttpError(response.status, response.statusText, payload);
  }

  return response;
}

async function githubJson(endpoint, options = {}) {
  const response = await githubRequest(endpoint, options);
  return response.json();
}

async function githubText(endpoint, options = {}) {
  const response = await githubRequest(endpoint, options);
  return response.text();
}

async function runShellCommand(command, repoRoot, context, options = {}) {
  const rendered = expandTemplate(command, context);
  console.log(`$ ${rendered}`);

  return new Promise((resolve, reject) => {
    const child = spawn(rendered, {
      cwd: repoRoot,
      env: loopEnvironment(context),
      shell: true,
      detached: true,
      stdio: ['ignore', 'pipe', 'pipe'],
    });
    activeStepChild = child;

    let stdout = '';
    let stderr = '';
    let timedOut = false;
    let killTimer = null;
    const timeoutMs = Number(options.timeoutMs ?? stepTimeoutMs);
    const timeoutTimer =
      timeoutMs > 0
        ? setTimeout(() => {
            timedOut = true;
            killStepGroup(child, 'SIGTERM');
            killTimer = setTimeout(() => killStepGroup(child, 'SIGKILL'), STEP_KILL_GRACE_MS);
          }, timeoutMs)
        : null;
    const settle = () => {
      if (timeoutTimer) clearTimeout(timeoutTimer);
      if (killTimer) clearTimeout(killTimer);
      if (activeStepChild === child) activeStepChild = null;
    };

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
      settle();
      reject(error);
    });

    child.on('close', (code) => {
      settle();
      if (timedOut) {
        reject(
          new Error(`${options.stepName ?? 'Command'} timed out after ${Math.round(timeoutMs / 60_000)} minute(s) and was stopped.`),
        );
        return;
      }
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

    const explicitOk = typeof parsed.ok === 'boolean' ? parsed.ok : result.code === 0;
    if (Array.isArray(parsed.findings)) {
      return {
        ...parsed,
        ok: explicitOk && parsed.findings.length === 0,
        tool: parsed.tool ?? fallbackToolName,
      };
    }

    return {
      ok: explicitOk,
      findings: [],
      raw: parsed,
      tool: parsed.tool ?? fallbackToolName,
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

function reportNeedsAttention(report) {
  return report.ok !== true || hasFindings(report);
}

function findingKey(finding) {
  return [finding.rule, finding.file, finding.message, finding.snippet].join('::');
}

function resolveBaseBranch(config = {}) {
  return normalizeGitBranchShortName(config.baseBranch) || DEFAULTS.baseBranch;
}

function resolveBranchPrefix(config = {}) {
  const raw = config.branchPrefix === undefined ? DEFAULTS.branchPrefix : config.branchPrefix;
  return String(raw ?? '').trim().replace(/^refs\/heads\//, '');
}

function isValidBranchPrefix(prefix) {
  return /^[A-Za-z0-9][A-Za-z0-9._-]*(\/[A-Za-z0-9][A-Za-z0-9._-]*)*[/-]$/.test(prefix) && !prefix.includes('..');
}

function buildIterationBranchName(config = {}, iteration = 1, now = new Date()) {
  const stamp = now.toISOString().replace(/[-:]/g, '').replace(/\..*$/, '').replace('T', '-');
  return `${resolveBranchPrefix(config)}${stamp}-${iteration}`;
}

async function isLinkedWorktree(repoRoot) {
  const gitDir = (await execText('git', ['rev-parse', '--git-dir'], repoRoot)).stdout.trim();
  const commonDir = (await execText('git', ['rev-parse', '--git-common-dir'], repoRoot)).stdout.trim();
  const resolvedGitDir = await fs.realpath(path.resolve(repoRoot, gitDir));
  const resolvedCommonDir = await fs.realpath(path.resolve(repoRoot, commonDir));
  return resolvedGitDir !== resolvedCommonDir;
}

// The loop never commits to, merges into, or pushes the base branch. Each iteration
// publishes its own branch and (optionally) a pull request, and discards its own failed
// attempts, so it must own the checkout it runs in.
async function validateLoopConfig(repoRoot, config = {}) {
  const pushingEnabled = !config.dryRun;
  const baseBranch = resolveBaseBranch(config);
  const branchPrefix = resolveBranchPrefix(config);

  if (Boolean(config.allowPushToMain) || isTruthyFlag(process.env[ALLOW_PUSH_TO_MAIN_ENV])) {
    throw new Error(
      `Pushing to ${baseBranch} is no longer supported: remove "allowPushToMain" and ${ALLOW_PUSH_TO_MAIN_ENV}. The loop publishes one branch and pull request per iteration instead.`,
    );
  }

  if (config.pushBranch) {
    throw new Error(
      '"pushBranch" is no longer supported. Configure "branchPrefix"; every iteration publishes a new branch under it.',
    );
  }

  if (!isValidBranchPrefix(branchPrefix)) {
    throw new Error(
      `Invalid branchPrefix "${branchPrefix}". Use a non-empty prefix ending in "/" or "-", for example "continuous-improvement-loop/".`,
    );
  }

  if (pushingEnabled && !config.verifyCommand) {
    throw new Error(
      'verifyCommand is required: the continuous improvement loop refuses to commit changes that were not built and tested.',
    );
  }

  const requireLinkedWorktree = config.requireLinkedWorktree !== false;
  if (pushingEnabled && requireLinkedWorktree && !(await isLinkedWorktree(repoRoot))) {
    throw new Error(
      `Continuous improvement loop must run in its own linked git worktree, not the primary checkout (${repoRoot}). Create one with "git worktree add --detach <path> ${config.pushRemote || DEFAULTS.pushRemote}/${baseBranch}" and start the loop there.`,
    );
  }

  return {
    baseBranch,
    branchPrefix,
    pushingEnabled,
    requireLinkedWorktree,
  };
}


async function getRemoteUrl(repoRoot, remoteName) {
  const { stdout } = await execText('git', ['remote', 'get-url', remoteName], repoRoot);
  return stdout.trim();
}

async function getHeadSha(repoRoot) {
  const { stdout } = await execText('git', ['rev-parse', 'HEAD'], repoRoot);
  return stdout.trim();
}

async function getRevisionSha(repoRoot, revision) {
  const { stdout } = await execText('git', ['rev-parse', revision], repoRoot);
  return stdout.trim();
}

async function getGitHubRemote(repoRoot, remoteName) {
  const remoteUrl = await getRemoteUrl(repoRoot, remoteName);
  const remote = parseGitHubRemote(remoteUrl);
  if (!remote) {
    throw new Error(`Unsupported Git remote for GitHub polling: ${remoteUrl}`);
  }
  return remote;
}

function splitLines(stdout) {
  return stdout
    .split('\n')
    .map((value) => value.replace(/\r$/, ''))
    .filter((value) => value.length > 0);
}

function normalizeGitBranchShortName(rawBranch) {
  const raw = String(rawBranch ?? '').trim();
  if (!raw || raw === 'HEAD' || raw === 'origin' || raw.endsWith('/HEAD')) return '';
  return raw
    .replace(/^refs\/heads\//, '')
    .replace(/^refs\/remotes\/origin\//, '')
    .replace(/^origin\//, '')
    .trim();
}

function extractPathsFromPorcelainLine(line) {
  const pathSpec = line.slice(3).trim();
  if (!pathSpec) return [];
  if (pathSpec.includes(' -> ')) {
    const [fromPath, toPath] = pathSpec.split(' -> ');
    return [fromPath, toPath].filter(Boolean);
  }
  return [pathSpec];
}

function extractPathsFromPorcelain(lines) {
  return [...new Set(lines.flatMap(extractPathsFromPorcelainLine))];
}

function filterTrackedChanges(lines, baselineTrackedPaths) {
  if (!baselineTrackedPaths || baselineTrackedPaths.size === 0) {
    return lines;
  }

  return lines.filter((line) =>
    extractPathsFromPorcelainLine(line).some((filePath) => !baselineTrackedPaths.has(filePath)),
  );
}

async function listTrackedChanges(repoRoot) {
  const { stdout } = await execText('git', ['-c', 'core.quotePath=false', 'status', '--porcelain', '--untracked-files=no'], repoRoot);
  return splitLines(stdout);
}

async function listUntrackedFiles(repoRoot) {
  const { stdout } = await execText('git', ['-c', 'core.quotePath=false', 'ls-files', '--others', '--exclude-standard'], repoRoot);
  return splitLines(stdout);
}

async function hasCommitCandidateChanges(repoRoot, baselineUntracked, baselineTrackedPaths) {
  const trackedChanges = filterTrackedChanges(await listTrackedChanges(repoRoot), baselineTrackedPaths);
  if (trackedChanges.length > 0) {
    return true;
  }

  const baseline = baselineUntracked ?? new Set();
  const untrackedFiles = await listUntrackedFiles(repoRoot);
  return untrackedFiles.some((filePath) => !baseline.has(filePath));
}

async function ensureStartState(repoRoot, allowDirty) {
  const trackedChanges = await listTrackedChanges(repoRoot);
  if (!allowDirty && trackedChanges.length > 0) {
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

  return {
    initialTrackedPaths: new Set(extractPathsFromPorcelain(trackedChanges)),
    initialUntracked,
  };
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

  const idea = await discoverImprovementIdea(repoRoot, { excludedFiles: config.pendingTargetFiles ?? [] });
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

async function generateLogicalReport(repoRoot, context, config) {
  if (config.logicalAuditCommand) {
    const result = await runShellCommand(config.logicalAuditCommand, repoRoot, context, {
      stepName: 'Logical correctness audit',
      allowFailure: true,
    });
    const report = parseReportOutput(result, 'custom-logical-audit');
    report.generatedAt = new Date().toISOString();
    await writeJson(context.logical_report_file, report);
    return report;
  }

  const allFindings = await collectLogicalFindings(repoRoot);
  const baseline = config.logicalBaselineFindingKeys ?? new Set();
  const findings = allFindings.filter((finding) => !baseline.has(findingKey(finding)));
  const report = {
    ok: findings.length === 0,
    findings,
    summary: summarizeLogicalFindings(findings),
    baselineSuppressedCount: allFindings.length - findings.length,
    baselineTotal: baseline.size,
    tool: 'builtin-logical-correctness-audit',
    generatedAt: new Date().toISOString(),
  };
  await writeJson(context.logical_report_file, report);
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

  const allFindings = await collectFormalFindings(repoRoot);
  const baseline = config.formalBaselineFindingKeys ?? new Set();
  const findings = allFindings.filter((finding) => !baseline.has(findingKey(finding)));
  const blockingFindings = collectFailingFormalFindings(findings, 'error');
  const loopModelCheck = verifyImprovementLoopModel();
  const report = {
    ok: blockingFindings.length === 0 && loopModelCheck.ok,
    findings,
    blockingFindings,
    loopModelCheck,
    summary: summarizeFormalFindings(findings),
    baselineSuppressedCount: allFindings.length - findings.length,
    baselineTotal: baseline.size,
    tool: 'builtin-formal-methods-audit',
    generatedAt: new Date().toISOString(),
  };
  await writeJson(context.formal_report_file, report);
  return report;
}

async function generateUxReport(repoRoot, context, config) {
  if (config.uxAuditCommand) {
    const result = await runShellCommand(config.uxAuditCommand, repoRoot, context, {
      stepName: 'UX quality audit',
      allowFailure: true,
    });
    const report = parseReportOutput(result, 'custom-ux-audit');
    report.generatedAt = new Date().toISOString();
    await writeJson(context.ux_report_file, report);
    return report;
  }

  const uxRoot = path.join(repoRoot, 'tdf-hq-ui', 'src');
  let allFindings = [];
  try {
    allFindings = await collectUxFindings(uxRoot);
  } catch {
    // UX audit is best-effort if the UI source tree is absent.
  }
  const baseline = config.uxBaselineFindingKeys ?? new Set();
  const findings = allFindings.filter((finding) => !baseline.has(findingKey(finding)));
  const report = {
    ok: findings.length === 0,
    findings,
    summary: summarizeUxFindings(findings),
    baselineSuppressedCount: allFindings.length - findings.length,
    baselineTotal: baseline.size,
    tool: 'builtin-ux-quality-audit',
    generatedAt: new Date().toISOString(),
  };
  await writeJson(context.ux_report_file, report);
  return report;
}

async function stageCommitCandidates(repoRoot, baselineUntracked, baselineTrackedPaths) {
  const trackedPaths = extractPathsFromPorcelain(
    filterTrackedChanges(await listTrackedChanges(repoRoot), baselineTrackedPaths),
  );
  if (trackedPaths.length > 0) {
    await execText('git', ['add', '-u', '--', ...trackedPaths], repoRoot);
  }
  const untrackedFiles = await listUntrackedFiles(repoRoot);
  const newUntrackedFiles = untrackedFiles.filter((filePath) => !baselineUntracked.has(filePath));
  if (newUntrackedFiles.length > 0) {
    await execText('git', ['add', '--', ...newUntrackedFiles], repoRoot);
  }
}

async function listStagedFiles(repoRoot, baselineUntracked, baselineTrackedPaths) {
  await stageCommitCandidates(repoRoot, baselineUntracked, baselineTrackedPaths);
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
  await execText('git', ['push', remoteName, `HEAD:refs/heads/${branchName}`], repoRoot);
}

async function syncSubmodulesRecursively(repoRoot, { checkout = false } = {}) {
  await execText('git', ['submodule', 'sync', '--recursive'], repoRoot);
  // --checkout pins every submodule to the revision the base records, even for submodules
  // configured with update=none, so gates that need the pinned sources see the right ones.
  const updateArgs = ['submodule', 'update', '--init', '--recursive'];
  if (checkout) updateArgs.push('--checkout');
  await execText('git', updateArgs, repoRoot);
}

function buildRemoteBranchRef(remoteName, branchName) {
  return `${remoteName}/${branchName}`;
}

function isMissingRemoteRefError(message) {
  return /\bcouldn't find remote ref\b|\bremote ref\b.*\bnot found\b/i.test(String(message ?? ''));
}

async function fetchBaseBranch(repoRoot, config) {
  const remoteName = config.pushRemote;
  const baseBranch = resolveBaseBranch(config);
  const remoteRef = buildRemoteBranchRef(remoteName, baseBranch);
  try {
    await execText('git', ['fetch', remoteName, baseBranch], repoRoot);
  } catch (error) {
    if (isMissingRemoteRefError(error.message)) {
      throw new Error(`Base branch ${remoteRef} does not exist on the remote.`);
    }
    throw new Error(`Failed to fetch ${remoteRef}.\n${error.message}`);
  }
  return {
    remoteRef,
    baseBranch,
    sha: await getRevisionSha(repoRoot, remoteRef),
  };
}

async function tryGit(repoRoot, args) {
  try {
    await execText('git', args, repoRoot);
    return true;
  } catch {
    return false;
  }
}

// Throw away everything the current iteration produced. Without --allow-dirty the loop
// owns the whole worktree, so a hard reset is exact. With --allow-dirty the operator's
// baseline changes are preserved and only paths the iteration touched are restored.
async function discardIterationChanges(repoRoot, config) {
  await tryGit(repoRoot, ['merge', '--abort']);
  await tryGit(repoRoot, ['rebase', '--abort']);
  await tryGit(repoRoot, ['cherry-pick', '--abort']);

  if (!config.allowDirty) {
    await execText('git', ['reset', '--hard', '--quiet'], repoRoot);
    await execText('git', ['clean', '-fd', '--quiet'], repoRoot);
    return;
  }

  await tryGit(repoRoot, ['reset', '--quiet']);
  const baselineTracked = config.initialTrackedPaths ?? new Set();
  const baselineUntracked = config.initialUntracked ?? new Set();
  const trackedPaths = extractPathsFromPorcelain(filterTrackedChanges(await listTrackedChanges(repoRoot), baselineTracked));
  for (const filePath of trackedPaths) {
    if (baselineTracked.has(filePath)) continue;
    await tryGit(repoRoot, ['checkout', '--quiet', 'HEAD', '--', filePath]);
  }
  for (const filePath of await listUntrackedFiles(repoRoot)) {
    if (baselineUntracked.has(filePath)) continue;
    await fs.rm(path.join(repoRoot, filePath), { force: true });
  }
}

async function deleteLocalLoopBranches(repoRoot, config) {
  const prefix = resolveBranchPrefix(config);
  const { stdout } = await execText('git', ['for-each-ref', '--format=%(refname:short)', `refs/heads/${prefix}`], repoRoot);
  for (const branchName of splitLines(stdout)) {
    // Only local leftovers are removed; published branches and their pull requests stay.
    await tryGit(repoRoot, ['branch', '-D', branchName]);
  }
}

async function prepareIterationBranch(repoRoot, config, iteration) {
  const base = await fetchBaseBranch(repoRoot, config);
  if (!config.allowDirty) {
    await discardIterationChanges(repoRoot, config);
  }
  await execText('git', ['checkout', '--quiet', '--detach', base.remoteRef], repoRoot);
  await deleteLocalLoopBranches(repoRoot, config);

  const branchName = buildIterationBranchName(config, iteration);
  await execText('git', ['checkout', '--quiet', '--no-track', '-b', branchName, base.remoteRef], repoRoot);
  await syncSubmodulesRecursively(repoRoot, { checkout: Boolean(config.checkoutSubmodules) });
  return {
    branchName,
    baseBranch: base.baseBranch,
    baseRef: base.remoteRef,
    baseSha: base.sha,
  };
}

async function leaveIterationBranch(repoRoot, branch) {
  await tryGit(repoRoot, ['checkout', '--quiet', '--detach', branch.baseRef]);
  await tryGit(repoRoot, ['branch', '-D', branch.branchName]);
}

async function countOpenLoopPullRequests(repoRoot, config) {
  const remote = await getGitHubRemote(repoRoot, config.pushRemote);
  const prefix = resolveBranchPrefix(config);
  let count = 0;
  for (let page = 1; page <= 10; page += 1) {
    const pulls = await githubJson(
      `repos/${remote.owner}/${remote.repo}/pulls?state=open&per_page=100&page=${page}`,
    );
    if (!Array.isArray(pulls) || pulls.length === 0) break;
    count += pulls.filter((pull) => String(pull?.head?.ref ?? '').startsWith(prefix)).length;
    if (pulls.length < 100) break;
  }
  return count;
}

function targetFile(target) {
  // "path/to/file.ext:12" -> "path/to/file.ext"
  return String(target ?? '').trim().replace(/:\d+(?::\d+)?$/, '');
}

function parsePullRequestTarget(body) {
  const match = /^- Target: (.+)$/m.exec(String(body ?? ''));
  const file = targetFile(match?.[1]);
  return file && file !== 'unspecified' ? file : '';
}

// Files the loop has already proposed a change for: open loop pull requests, and ones a
// person closed without merging (re-proposing a rejected idea would only repeat the noise).
// A merged proposal removes its finding from the base, so it needs no entry here.
async function listPendingTargetFiles(repoRoot, config) {
  const remote = await getGitHubRemote(repoRoot, config.pushRemote);
  const prefix = resolveBranchPrefix(config);
  const files = new Set();
  for (let page = 1; page <= 3; page += 1) {
    const pulls = await githubJson(
      `repos/${remote.owner}/${remote.repo}/pulls?state=all&sort=created&direction=desc&per_page=100&page=${page}`,
    );
    if (!Array.isArray(pulls) || pulls.length === 0) break;
    for (const pull of pulls) {
      if (!String(pull?.head?.ref ?? '').startsWith(prefix)) continue;
      if (pull.state !== 'open' && pull.merged_at) continue;
      const file = parsePullRequestTarget(pull.body);
      if (file) files.add(file);
    }
    if (pulls.length < 100) break;
  }
  return [...files].sort();
}

function buildPullRequestBody(config, context, branch) {
  const lines = [
    'Opened automatically by the continuous improvement loop. It stays a draft until a person reviews it.',
    '',
    `- Idea: ${context.idea_title || '(untitled)'}`,
    `- Source: ${context.idea_source || 'unknown'}`,
    `- Target: ${context.idea_target || 'unspecified'}`,
    `- Reason: ${context.idea_reason || 'unspecified'}`,
    `- Base: ${branch.baseBranch} at ${branch.baseSha}`,
    `- Local verification before commit: \`${config.verifyCommand}\` passed`,
  ];
  return `${lines.join('\n')}\n`;
}

async function openPullRequest(repoRoot, config, context, branch) {
  const remote = await getGitHubRemote(repoRoot, config.pushRemote);
  const title = String(context.last_commit_message ?? '').split('\n')[0].trim() || `Continuous improvement: ${branch.branchName}`;
  const payload = await githubJson(`repos/${remote.owner}/${remote.repo}/pulls`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({
      title,
      head: branch.branchName,
      base: branch.baseBranch,
      body: buildPullRequestBody(config, context, branch),
      draft: config.draftPullRequest !== false,
    }),
  });
  return {
    number: payload.number ?? null,
    url: payload.html_url ?? '',
  };
}

async function runVerification(repoRoot, config, context) {
  const result = await runShellCommand(config.verifyCommand, repoRoot, context, {
    stepName: 'Verification',
    allowFailure: true,
  });
  if (result.code !== 0) {
    throw new Error(`Verification failed with exit code ${result.code}; the iteration's changes were not committed.`);
  }
}


async function fetchCheckRuns(owner, repo, sha) {
  const payload = await githubJson(`repos/${owner}/${repo}/commits/${encodeURIComponent(sha)}/check-runs`);
  return payload.check_runs ?? [];
}

async function fetchWorkflowRuns(owner, repo, sha) {
  const payload = await githubJson(
    `repos/${owner}/${repo}/actions/runs?head_sha=${encodeURIComponent(sha)}&per_page=100`,
  );
  return payload.workflow_runs ?? [];
}

async function fetchWorkflowJobs(owner, repo, runId) {
  const payload = await githubJson(`repos/${owner}/${repo}/actions/runs/${runId}/jobs?per_page=100`);
  return payload.jobs ?? [];
}

function isSuccessfulConclusion(conclusion) {
  return conclusion === 'success' || conclusion === 'neutral' || conclusion === 'skipped';
}

function isFailedConclusion(conclusion) {
  return Boolean(conclusion) && !isSuccessfulConclusion(conclusion);
}

function trimLogForReport(logText) {
  const normalized = String(logText ?? '').replace(/\r/g, '');
  const lines = normalized.split('\n');
  const tail = lines.slice(-GITHUB_LOG_LINE_LIMIT).join('\n');
  if (tail.length <= GITHUB_LOG_CHAR_LIMIT && lines.length <= GITHUB_LOG_LINE_LIMIT) {
    return tail.trim();
  }

  const clipped = tail.length > GITHUB_LOG_CHAR_LIMIT ? tail.slice(-GITHUB_LOG_CHAR_LIMIT) : tail;
  return `[truncated to last ${GITHUB_LOG_LINE_LIMIT} lines / ${GITHUB_LOG_CHAR_LIMIT} chars]\n${clipped}`.trim();
}

async function fetchCheckRunAnnotations(checkRun) {
  if (!checkRun?.output?.annotations_count || !checkRun.output.annotations_url) {
    return [];
  }

  const annotations = await githubJson(checkRun.output.annotations_url);
  return Array.isArray(annotations) ? annotations.slice(0, 50) : [];
}

async function fetchFailedCheckDiagnostics(checkRun) {
  let annotations = [];
  let annotationError = '';
  try {
    annotations = await fetchCheckRunAnnotations(checkRun);
  } catch (error) {
    annotationError = error.message;
  }

  return {
    id: checkRun.id ?? 0,
    name: checkRun.name ?? '(unnamed check)',
    appSlug: checkRun.app?.slug ?? '',
    appName: checkRun.app?.name ?? '',
    status: checkRun.status ?? 'unknown',
    conclusion: checkRun.conclusion ?? 'pending',
    detailsUrl: checkRun.details_url ?? checkRun.html_url ?? '',
    htmlUrl: checkRun.html_url ?? '',
    outputTitle: checkRun.output?.title ?? '',
    outputSummary: htmlToText(checkRun.output?.summary ?? ''),
    outputText: htmlToText(checkRun.output?.text ?? ''),
    annotations: annotations.map((annotation) => ({
      path: annotation.path ?? '',
      startLine: annotation.start_line ?? null,
      endLine: annotation.end_line ?? null,
      level: annotation.annotation_level ?? '',
      title: annotation.title ?? '',
      message: annotation.message ?? '',
    })),
    annotationError: annotationError || undefined,
  };
}

async function fetchFailedWorkflowDiagnostics(owner, repo, workflowRun) {
  const jobs = await fetchWorkflowJobs(owner, repo, workflowRun.id);
  const failingJobs = jobs.filter((job) => job.status !== 'completed' || isFailedConclusion(job.conclusion));

  const jobDiagnostics = await Promise.all(
    failingJobs.map(async (job) => {
      let logExcerpt = '';
      let logError = '';
      try {
        logExcerpt = trimLogForReport(
          await githubText(`repos/${owner}/${repo}/actions/jobs/${job.id}/logs`, {
            headers: { Accept: 'application/vnd.github+json' },
          }),
        );
      } catch (error) {
        logError = error.message;
      }

      return {
        id: job.id ?? 0,
        name: job.name ?? '(unnamed job)',
        status: job.status ?? 'unknown',
        conclusion: job.conclusion ?? 'pending',
        htmlUrl: job.html_url ?? '',
        failedSteps: (job.steps ?? [])
          .filter((step) => step.status !== 'completed' || isFailedConclusion(step.conclusion))
          .map((step) => ({
            number: step.number ?? 0,
            name: step.name ?? '',
            status: step.status ?? 'unknown',
            conclusion: step.conclusion ?? 'pending',
          })),
        logExcerpt: logExcerpt || undefined,
        logError: logError || undefined,
      };
    }),
  );

  return {
    id: workflowRun.id ?? 0,
    name: workflowRun.name ?? workflowRun.display_title ?? '(unnamed workflow)',
    status: workflowRun.status ?? 'unknown',
    conclusion: workflowRun.conclusion ?? 'pending',
    htmlUrl: workflowRun.html_url ?? '',
    jobs: jobDiagnostics,
  };
}

function emptyCiSummary() {
  return {
    checks: {
      pending: [],
      successful: [],
      failed: [],
    },
    workflows: {
      pending: [],
      successful: [],
      failed: [],
    },
  };
}

function isRetryableGitHubPollingError(error) {
  if (error instanceof GitHubHttpError) {
    return error.status === 408 || error.status === 425 || error.status === 429 || error.status >= 500;
  }

  const message = String(error?.message ?? '');
  return /\b(fetch failed|network|timeout|timed out|socket hang up|econnreset|enotfound|eai_again|etimedout)\b/i.test(
    message,
  );
}

async function fetchFailedCheckDiagnosticsSafely(checkRun) {
  try {
    return await fetchFailedCheckDiagnostics(checkRun);
  } catch (error) {
    return {
      id: checkRun.id ?? 0,
      name: checkRun.name ?? '(unnamed check)',
      appSlug: checkRun.app?.slug ?? '',
      appName: checkRun.app?.name ?? '',
      status: checkRun.status ?? 'unknown',
      conclusion: checkRun.conclusion ?? 'pending',
      detailsUrl: checkRun.details_url ?? checkRun.html_url ?? '',
      htmlUrl: checkRun.html_url ?? '',
      diagnosticsError: error.message,
    };
  }
}

async function fetchFailedWorkflowDiagnosticsSafely(owner, repo, workflowRun) {
  try {
    return await fetchFailedWorkflowDiagnostics(owner, repo, workflowRun);
  } catch (error) {
    return {
      id: workflowRun.id ?? 0,
      name: workflowRun.name ?? workflowRun.display_title ?? '(unnamed workflow)',
      status: workflowRun.status ?? 'unknown',
      conclusion: workflowRun.conclusion ?? 'pending',
      htmlUrl: workflowRun.html_url ?? '',
      diagnosticsError: error.message,
      jobs: [],
    };
  }
}

async function waitForGreenCi(repoRoot, config, sha) {
  const deadline = Date.now() + Number(config.ciTimeoutMinutes) * 60_000;
  const remote = await getGitHubRemote(repoRoot, config.pushRemote);
  let sawChecks = false;
  const pollErrors = [];

  while (true) {
    let checkRuns;
    let workflowRuns;
    let checkSummary;
    let workflowSummary;
    try {
      checkRuns = await fetchCheckRuns(remote.owner, remote.repo, sha);
      workflowRuns = await fetchWorkflowRuns(remote.owner, remote.repo, sha);
      checkSummary = summarizeCheckRuns(checkRuns);
      workflowSummary = summarizeWorkflowRuns(workflowRuns);
    } catch (error) {
      if (!isRetryableGitHubPollingError(error)) {
        throw error;
      }

      const pollError = {
        at: new Date().toISOString(),
        message: error.message,
      };
      pollErrors.push(pollError);
      console.warn(`GitHub polling request failed for ${sha}: ${error.message}`);

      if (Date.now() > deadline) {
        return {
          ok: false,
          owner: remote.owner,
          repo: remote.repo,
          sha,
          summary: emptyCiSummary(),
          checkRuns: [],
          workflowRuns: [],
          timeout: true,
          message: 'Timed out waiting for GitHub checks after repeated polling request failures.',
          pollErrors,
        };
      }

      await sleep(Number(config.pollIntervalSeconds) * 1000);
      continue;
    }
    const failedCheckRuns = checkRuns.filter(
      (checkRun) => checkRun.status === 'completed' && isFailedConclusion(checkRun.conclusion),
    );
    const failedWorkflowRuns = workflowRuns.filter(
      (workflowRun) => workflowRun.status === 'completed' && isFailedConclusion(workflowRun.conclusion),
    );
    const pendingCount = checkSummary.pending.length + workflowSummary.pending.length;

    if (checkRuns.length > 0 || workflowRuns.length > 0) {
      sawChecks = true;
    }

    if (failedCheckRuns.length > 0 || failedWorkflowRuns.length > 0) {
      return {
        ok: false,
        owner: remote.owner,
        repo: remote.repo,
        sha,
        summary: {
          checks: checkSummary,
          workflows: workflowSummary,
        },
        checkRuns,
        workflowRuns,
        pollErrors: pollErrors.length > 0 ? pollErrors : undefined,
        failedChecks: await Promise.all(failedCheckRuns.map((checkRun) => fetchFailedCheckDiagnosticsSafely(checkRun))),
        failedWorkflowRuns: await Promise.all(
          failedWorkflowRuns.map((workflowRun) =>
            fetchFailedWorkflowDiagnosticsSafely(remote.owner, remote.repo, workflowRun)
          ),
        ),
      };
    }

    if ((pendingCount > 0 || !sawChecks) && Date.now() <= deadline) {
      console.log(
        sawChecks
          ? `Waiting for ${pendingCount} GitHub check(s) / workflow run(s) to finish for ${sha}...`
          : `Waiting for GitHub check runs to appear for ${sha}...`,
      );
      await sleep(Number(config.pollIntervalSeconds) * 1000);
      continue;
    }

    if (!sawChecks) {
      return {
        ok: false,
        owner: remote.owner,
        repo: remote.repo,
        sha,
        summary: {
          checks: checkSummary,
          workflows: workflowSummary,
        },
        checkRuns,
        workflowRuns,
        timeout: true,
        message: 'Timed out waiting for GitHub check runs to appear.',
        pollErrors: pollErrors.length > 0 ? pollErrors : undefined,
      };
    }

    if (pendingCount > 0) {
      return {
        ok: false,
        owner: remote.owner,
        repo: remote.repo,
        sha,
        summary: {
          checks: checkSummary,
          workflows: workflowSummary,
        },
        checkRuns,
        workflowRuns,
        timeout: true,
        message: 'Timed out waiting for GitHub checks to finish.',
        pollErrors: pollErrors.length > 0 ? pollErrors : undefined,
      };
    }

    return {
      ok: true,
      owner: remote.owner,
      repo: remote.repo,
      sha,
      summary: {
        checks: checkSummary,
        workflows: workflowSummary,
      },
      checkRuns,
      workflowRuns,
      pollErrors: pollErrors.length > 0 ? pollErrors : undefined,
    };
  }
}

async function pollBaseBranchCi(repoRoot, config, context, branch) {
  context.head_sha = branch.baseSha;
  if (!config.pollGitHub) {
    return null;
  }

  const ciResult = await waitForGreenCi(repoRoot, config, branch.baseSha);
  await writeJson(context.ci_report_file, ciResult);
  return ciResult;
}

async function finalizeIteration(repoRoot, config, context, branch) {
  if (config.dryRun) {
    const trackedChanges = filterTrackedChanges(
      await listTrackedChanges(repoRoot),
      config.initialTrackedPaths,
    );
    const newUntrackedFiles = (await listUntrackedFiles(repoRoot)).filter(
      (filePath) => !config.initialUntracked.has(filePath),
    );
    return {
      ok: true,
      dryRun: true,
      changes: [...trackedChanges, ...newUntrackedFiles.map((filePath) => `?? ${filePath}`)],
    };
  }

  let pullRequest = null;
  let repairAttempts = 0;

  while (true) {
    if (!(await hasCommitCandidateChanges(repoRoot, config.initialUntracked, config.initialTrackedPaths))) {
      return {
        ok: true,
        skipped: true,
        reason: 'No changes detected after this iteration.',
      };
    }

    // Nothing is committed, pushed, or proposed until the build-and-test gate passes.
    await emitLoopStatus({ state: 'running', phase: 'verification', ideaTitle: context.idea_title });
    logStep('Verification');
    await runVerification(repoRoot, config, context);

    const stagedFiles = await listStagedFiles(
      repoRoot,
      config.initialUntracked,
      config.initialTrackedPaths,
    );
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
    context.last_commit_message = context.last_commit_message || commitMessage;
    console.log(`Committed ${commitResult.sha} with ${commitResult.stagedFiles.length} file(s).`);

    console.log(`Pushing ${commitResult.sha} to ${config.pushRemote}:${branch.branchName}`);
    await pushHead(repoRoot, config.pushRemote, branch.branchName);

    if (config.openPullRequest && !pullRequest) {
      pullRequest = await openPullRequest(repoRoot, config, context, branch);
      console.log(`Opened pull request ${pullRequest.url || `#${pullRequest.number}`}.`);
    }

    const published = {
      ok: true,
      pushed: true,
      sha: commitResult.sha,
      branch: branch.branchName,
      pullRequest,
    };

    if (!config.pollGitHub) {
      return published;
    }

    const ciResult = await waitForGreenCi(repoRoot, config, commitResult.sha);
    await writeJson(context.ci_report_file, ciResult);

    if (ciResult.ok) {
      return { ...published, ciResult };
    }

    if (!config.ciRepairCommand) {
      throw new Error(
        `GitHub checks failed for ${commitResult.sha}. Configure ciRepairCommand or inspect ${context.ci_report_file}.`,
      );
    }

    repairAttempts += 1;
    if (repairAttempts > Number(config.maxCiRepairAttempts)) {
      throw new Error(
        `GitHub checks still fail for ${branch.branchName} after ${config.maxCiRepairAttempts} repair attempt(s); leaving it for review.`,
      );
    }

    logStep('CI repair');
    await runShellCommand(config.ciRepairCommand, repoRoot, context, {
      stepName: 'CI repair',
    });

    if (!(await hasCommitCandidateChanges(repoRoot, config.initialUntracked, config.initialTrackedPaths))) {
      throw new Error('ciRepairCommand completed without creating changes.');
    }
  }
}

async function runIteration(repoRoot, config, iteration) {
  const contextDir = await fs.mkdtemp(path.join(os.tmpdir(), 'continuous-improvement-loop-'));
  const context = buildContext(repoRoot, contextDir, iteration);

  if (config.dryRun) {
    // A dry run never switches branches or discards anything.
    const head = await getHeadSha(repoRoot);
    config.pendingTargetFiles = [];
    await writeJson(context.excluded_targets_file, []);
    const dryBranch = { branchName: '', baseBranch: resolveBaseBranch(config), baseRef: head, baseSha: head };
    return runIterationSteps(repoRoot, config, iteration, context, dryBranch);
  }

  if (config.openPullRequest && Number(config.maxOpenPullRequests) > 0) {
    const openCount = await countOpenLoopPullRequests(repoRoot, config);
    if (openCount >= Number(config.maxOpenPullRequests)) {
      const reason = `${openCount} loop pull request(s) are already open (limit ${config.maxOpenPullRequests}); waiting for review before proposing more.`;
      console.log(reason);
      await emitLoopStatus({ state: 'running', phase: 'waiting-for-review', currentIteration: iteration, details: reason });
      await fs.rm(contextDir, { recursive: true, force: true });
      await sleep(Number(config.prBacklogDelaySeconds) * 1000);
      return { ok: true, skipped: true, backlog: true, reason };
    }
  }

  config.pendingTargetFiles = config.openPullRequest ? await listPendingTargetFiles(repoRoot, config) : [];
  await writeJson(context.excluded_targets_file, config.pendingTargetFiles);

  await emitLoopStatus({ state: 'running', phase: 'branch-preparation', currentIteration: iteration, contextDir });
  const branch = await prepareIterationBranch(repoRoot, config, iteration);
  context.branch_name = branch.branchName;
  console.log(`Iteration ${iteration} works on ${branch.branchName} from ${branch.baseRef} (${branch.baseSha}).`);

  let succeeded = false;
  try {
    const result = await runIterationSteps(repoRoot, config, iteration, context, branch);
    succeeded = true;
    return result;
  } catch (error) {
    try {
      await discardIterationChanges(repoRoot, config);
    } catch (discardError) {
      console.error(`Failed to discard iteration changes: ${discardError.message}`);
    }
    throw error;
  } finally {
    await leaveIterationBranch(repoRoot, branch);
    if (succeeded) {
      await fs.rm(contextDir, { recursive: true, force: true });
    }
  }
}

async function runIterationSteps(repoRoot, config, iteration, context, branch) {
  const contextDir = context.context_dir;
  const baseCiResult = await pollBaseBranchCi(repoRoot, config, context, branch);

  let repairLatestCommitOnly = false;
  if (baseCiResult && !baseCiResult.ok) {
    if (!config.ciRepairCommand) {
      throw new Error(
        `Latest ${branch.baseBranch} commit ${branch.baseSha} has failing GitHub checks. Configure ciRepairCommand or inspect ${context.ci_report_file}.`,
      );
    }

    repairLatestCommitOnly = true;
    context.idea_source = 'github-ci';
    context.idea_title = `Repair GitHub CI for ${branch.baseSha.slice(0, 7)}`;
    context.idea_target = branch.baseBranch;
    context.idea_reason = `Fix failing GitHub Actions on latest remote commit ${branch.baseSha}.`;

    await emitLoopStatus({
      state: 'running',
      phase: 'latest-commit-ci-repair',
      currentIteration: iteration,
      contextDir,
      ideaTitle: context.idea_title,
    });
    logStep(`Iteration ${iteration}: latest remote commit CI repair`);
    await runShellCommand(config.ciRepairCommand, repoRoot, context, {
      stepName: 'Latest commit CI repair',
    });

    if (!(await hasCommitCandidateChanges(repoRoot, config.initialUntracked, config.initialTrackedPaths))) {
      throw new Error('ciRepairCommand completed without creating changes for the latest remote commit failure.');
    }
  }

  // Phase 1: Logical correctness audit (whole codebase) — highest priority
  await emitLoopStatus({ state: 'running', phase: 'logical-audit', currentIteration: iteration, ideaTitle: context.idea_title });
  logStep(`Iteration ${iteration}: logical correctness audit`);
  let logicalReport = await generateLogicalReport(repoRoot, context, config);
  if (reportNeedsAttention(logicalReport)) {
    if (!config.logicalFixCommand) {
      throw new Error(`Logical correctness audit did not pass. Configure logicalFixCommand or inspect ${context.logical_report_file}.`);
    }

    await emitLoopStatus({ state: 'running', phase: 'logical-fixes', currentIteration: iteration, ideaTitle: context.idea_title });
    logStep(`Iteration ${iteration}: logical fixes`);
    await runShellCommand(config.logicalFixCommand, repoRoot, context, {
      stepName: 'Logical fixes',
    });

    logicalReport = await generateLogicalReport(repoRoot, context, config);
    if (reportNeedsAttention(logicalReport)) {
      throw new Error(`Logical correctness audit still does not pass after logicalFixCommand. See ${context.logical_report_file}.`);
    }
  }

  // Phase 2: Formal methods audit (invariants, preconditions, type contracts)
  await emitLoopStatus({ state: 'running', phase: 'formal-audit', currentIteration: iteration, ideaTitle: context.idea_title });
  logStep(`Iteration ${iteration}: formal methods audit`);
  let formalReport = await generateFormalReport(repoRoot, context, config);
  if (reportNeedsAttention(formalReport)) {
    if (!config.formalFixCommand) {
      throw new Error(
        `Formal verification did not pass. Configure formalFixCommand or inspect ${context.formal_report_file}.`,
      );
    }

    await emitLoopStatus({ state: 'running', phase: 'formal-fixes', currentIteration: iteration, ideaTitle: context.idea_title });
    logStep(`Iteration ${iteration}: formal fixes`);
    await runShellCommand(config.formalFixCommand, repoRoot, context, {
      stepName: 'Formal fixes',
    });

    formalReport = await generateFormalReport(repoRoot, context, config);
    if (reportNeedsAttention(formalReport)) {
      throw new Error(`Formal verification still does not pass after formalFixCommand. See ${context.formal_report_file}.`);
    }
  }

  // Phase 3: Idea discovery and implementation (only if we are not in CI-repair-only mode)
  if (!repairLatestCommitOnly) {
    await emitLoopStatus({ state: 'running', phase: 'idea-discovery', currentIteration: iteration, contextDir });
    logStep(`Iteration ${iteration}: idea discovery`);
    const idea = await generateIdea(repoRoot, context, config);
    context.idea_source = idea.source ?? '';
    context.idea_title = idea.title ?? '';
    context.idea_target = idea.target ?? '';
    context.idea_reason = idea.reason ?? '';

    const ideaFile = targetFile(context.idea_target);
    if (ideaFile && (config.pendingTargetFiles ?? []).includes(ideaFile)) {
      throw new Error(`Idea targets ${ideaFile}, which already has a loop pull request awaiting review; skipping it.`);
    }

    if (!config.implementationCommand) {
      throw new Error('implementationCommand is required to run the loop unattended.');
    }

    await emitLoopStatus({ state: 'running', phase: 'implementation', currentIteration: iteration, ideaTitle: context.idea_title });
    logStep(`Iteration ${iteration}: implementation`);
    if (config.shutdownSimulatorsBeforeImplementation) {
      await shutdownBootedSimulators();
    }
    await runShellCommand(config.implementationCommand, repoRoot, context, {
      stepName: 'Implementation',
    });
  }

  // Phase 4: UX quality audit (simplicity, minimalism, intuitiveness, engagement)
  await emitLoopStatus({ state: 'running', phase: 'ux-audit', currentIteration: iteration, ideaTitle: context.idea_title });
  logStep(`Iteration ${iteration}: UX quality audit`);
  let uxReport = await generateUxReport(repoRoot, context, config);
  if (reportNeedsAttention(uxReport)) {
    if (!config.uxFixCommand) {
      throw new Error(`UX quality audit did not pass. Configure uxFixCommand or inspect ${context.ux_report_file}.`);
    }

    await emitLoopStatus({ state: 'running', phase: 'ux-fixes', currentIteration: iteration, ideaTitle: context.idea_title });
    logStep(`Iteration ${iteration}: UX fixes`);
    await runShellCommand(config.uxFixCommand, repoRoot, context, {
      stepName: 'UX fixes',
    });

    uxReport = await generateUxReport(repoRoot, context, config);
    if (reportNeedsAttention(uxReport)) {
      throw new Error(`UX quality audit still does not pass after uxFixCommand. See ${context.ux_report_file}.`);
    }
  }

  // Phase 5: Legacy UI accessibility audit (kept as a safety net)
  await emitLoopStatus({ state: 'running', phase: 'ui-audit', currentIteration: iteration, ideaTitle: context.idea_title });
  logStep(`Iteration ${iteration}: UI accessibility audit`);
  let uiReport = await generateUiReport(repoRoot, context, config);
  if (reportNeedsAttention(uiReport)) {
    if (!config.uiFixCommand) {
      throw new Error(`UI audit did not pass. Configure uiFixCommand or inspect ${context.ui_report_file}.`);
    }

    await emitLoopStatus({ state: 'running', phase: 'ui-fixes', currentIteration: iteration, ideaTitle: context.idea_title });
    logStep(`Iteration ${iteration}: UI fixes`);
    await runShellCommand(config.uiFixCommand, repoRoot, context, {
      stepName: 'UI fixes',
    });

    uiReport = await generateUiReport(repoRoot, context, config);
    if (reportNeedsAttention(uiReport)) {
      throw new Error(`UI audit still does not pass after uiFixCommand. See ${context.ui_report_file}.`);
    }
  }

  await emitLoopStatus({ state: 'running', phase: 'verify-commit-push', currentIteration: iteration, ideaTitle: context.idea_title });
  logStep(`Iteration ${iteration}: verify, commit, push, and pull request`);
  const result = await finalizeIteration(repoRoot, config, context, branch);
  await emitLoopStatus({ state: 'running', phase: 'iteration-complete', currentIteration: iteration, lastIterationResult: result.ok ? 'ok' : 'attention', lastError: null });
  return result;
}


function printHelp() {
  console.log(`Usage: node scripts/continuous-improvement-loop.mjs [options]

Options:
  --config <path>           JSON config file with command hooks and git settings.
  --max-iterations <n>      0 means infinite. Default: 0.
  --allow-dirty             Allow starting from a dirty worktree.
  --dry-run                 Run the loop without committing or pushing.
  --validate-config-only    Validate config and exit without running an iteration.
  --help                    Show this help.

Placeholders available inside command hooks:
  {repo_root} {tool_root} {context_dir} {iteration}
  {idea_file} {logical_report_file} {formal_report_file} {ux_report_file} {ui_report_file} {ci_report_file}

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
      'validate-config-only': { type: 'boolean' },
      help: { type: 'boolean' },
    },
  });

  if (values.help) {
    printHelp();
    return;
  }

  const repoRoot = process.cwd();
  await emitLoopStatus({ state: 'running', phase: 'startup', repoRoot, currentIteration: 0 });
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

  const validatedConfig = await validateLoopConfig(repoRoot, config);
  if (values['validate-config-only']) {
    console.log(
      `Continuous improvement loop config OK. baseBranch=${validatedConfig.baseBranch} branchPrefix=${validatedConfig.branchPrefix} pushingEnabled=${String(validatedConfig.pushingEnabled)} openPullRequest=${String(Boolean(config.openPullRequest))}`,
    );
    return;
  }

  if (!Number.isFinite(Number(config.maxIterations)) || Number(config.maxIterations) < 0) {
    throw new Error('maxIterations must be a non-negative number.');
  }

  stepTimeoutMs = Math.max(0, Number(config.stepTimeoutMinutes) || 0) * 60_000;
  for (const signal of ['SIGTERM', 'SIGINT', 'SIGHUP']) {
    process.on(signal, () => {
      // Stop the running worker tree before exiting so nothing keeps editing the worktree.
      const child = activeStepChild;
      killStepGroup(child, 'SIGTERM');
      setTimeout(() => {
        killStepGroup(child, 'SIGKILL');
        process.exit(143);
      }, child ? 3000 : 0);
    });
  }

  if (
    !config.dryRun &&
    !config.allowDirty &&
    validatedConfig.requireLinkedWorktree &&
    (await listTrackedChanges(repoRoot)).length + (await listUntrackedFiles(repoRoot)).length > 0
  ) {
    // Leftovers from an interrupted run in the loop's own worktree are unverified work.
    console.warn('Discarding uncommitted leftovers from an interrupted run in the loop worktree.');
    await discardIterationChanges(repoRoot, config);
  }

  const startState = await ensureStartState(repoRoot, config.allowDirty);
  config.initialTrackedPaths = startState.initialTrackedPaths;
  config.initialUntracked = startState.initialUntracked;
  config.logicalBaselineFindingKeys = new Set();
  config.formalBaselineFindingKeys = new Set();
  config.uxBaselineFindingKeys = new Set();
  config.uiBaselineFindingKeys = new Set();

  try {
    const logicalFindings = await collectLogicalFindings(repoRoot);
    for (const finding of logicalFindings) {
      config.logicalBaselineFindingKeys.add(findingKey(finding));
    }
    if (config.logicalBaselineFindingKeys.size > 0) {
      console.log(`Recorded ${config.logicalBaselineFindingKeys.size} baseline logical finding(s) to avoid re-fixing the existing backlog in one iteration.`);
    }
  } catch (error) {
    console.warn(`Skipping logical baseline capture: ${error.message}`);
  }

  try {
    const formalFindings = await collectFormalFindings(repoRoot);
    for (const finding of formalFindings) {
      config.formalBaselineFindingKeys.add(findingKey(finding));
    }
    if (config.formalBaselineFindingKeys.size > 0) {
      console.log(`Recorded ${config.formalBaselineFindingKeys.size} baseline formal finding(s) to avoid re-fixing the existing backlog in one iteration.`);
    }
  } catch (error) {
    console.warn(`Skipping formal baseline capture: ${error.message}`);
  }

  try {
    const uxFindings = await collectUxFindings(path.join(repoRoot, 'tdf-hq-ui', 'src'));
    for (const finding of uxFindings) {
      config.uxBaselineFindingKeys.add(findingKey(finding));
    }
    if (config.uxBaselineFindingKeys.size > 0) {
      console.log(`Recorded ${config.uxBaselineFindingKeys.size} baseline UX finding(s) to avoid re-fixing the existing backlog in one iteration.`);
    }
  } catch (error) {
    console.warn(`Skipping UX baseline capture: ${error.message}`);
  }

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
  let consecutiveFailures = 0;
  while (config.maxIterations === 0 || iteration <= Number(config.maxIterations)) {
    let result;
    try {
      result = await runIteration(repoRoot, config, iteration);
      consecutiveFailures = 0;
    } catch (error) {
      // A failed iteration is discarded, never committed. The loop moves on to the next
      // idea unless failures keep repeating, which points at an environment problem.
      consecutiveFailures += 1;
      const isLastIteration = config.maxIterations !== 0 && iteration >= Number(config.maxIterations);
      const tooManyFailures =
        Number(config.maxConsecutiveFailures) > 0 && consecutiveFailures >= Number(config.maxConsecutiveFailures);
      if (config.dryRun || isLastIteration || tooManyFailures) {
        throw error;
      }
      console.error(`Iteration ${iteration} failed and was discarded: ${error.message}`);
      await emitLoopStatus({
        state: 'running',
        phase: 'iteration-failed',
        currentIteration: iteration,
        lastIterationResult: 'discarded',
        lastError: error.message,
      });
      await sleep(Number(config.failureDelaySeconds) * 1000);
      iteration += 1;
      continue;
    }
    if (result.skipped && !result.backlog && config.stopOnNoChanges) {
      console.log(result.reason);
      break;
    }
    if (Number(config.iterationDelaySeconds) > 0) {
      console.log(`Sleeping ${config.iterationDelaySeconds}s before the next iteration...`);
      await emitLoopStatus({ state: 'running', phase: 'sleeping', currentIteration: iteration });
      await sleep(Number(config.iterationDelaySeconds) * 1000);
    }
    iteration += 1;
    await emitLoopStatus({ state: 'running', phase: 'iteration-start', currentIteration: iteration });
  }

  await emitLoopStatus({ state: 'exited', phase: 'completed', currentIteration: iteration - 1 });
}

export {
  main,
  discoverImprovementIdea,
  buildIterationBranchName,
  buildPullRequestBody,
  countOpenLoopPullRequests,
  discardIterationChanges,
  listPendingTargetFiles,
  isLinkedWorktree,
  openPullRequest,
  prepareIterationBranch,
  validateLoopConfig,
  syncSubmodulesRecursively,
  waitForGreenCi,
};

function isCliEntry() {
  if (!process.argv[1]) {
    return false;
  }

  return import.meta.url === pathToFileURL(path.resolve(process.argv[1])).href;
}

if (isCliEntry()) {
  main().catch(async (error) => {
    await emitLoopStatus({
      state: 'crashed',
      phase: 'error',
      lastError: error.message,
    });
    console.error(error.message);
    process.exit(1);
  });
}
