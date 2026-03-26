#!/usr/bin/env node
import fs from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';
import process from 'node:process';
import { execFile, spawn } from 'node:child_process';
import { pathToFileURL } from 'node:url';
import { promisify, parseArgs } from 'node:util';
import { discoverImprovementIdea } from './lib/discovery.mjs';
import { collectUiFindings, summarizeUiFindings } from './lib/ui-static-audit.mjs';
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
  stopOnNoChanges: true,
  iterationDelaySeconds: 10,
  commitMessageTemplate: '{commit_message}',
};

function sleep(milliseconds) {
  return new Promise((resolve) => {
    setTimeout(resolve, milliseconds);
  });
}

function getGitHubToken() {
  return process.env.GITHUB_TOKEN ?? process.env.GH_TOKEN ?? process.env.GITHUB_PAT ?? '';
}

function getGitHubApiBase() {
  return (process.env.GITHUB_API_BASE_URL ?? 'https://api.github.com').replace(/\/$/, '');
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
  const { stdout } = await execText('git', ['status', '--porcelain', '--untracked-files=no'], repoRoot);
  return splitLines(stdout);
}

async function listUntrackedFiles(repoRoot) {
  const { stdout } = await execText('git', ['ls-files', '--others', '--exclude-standard'], repoRoot);
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

  const idea = await discoverImprovementIdea(repoRoot);
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
  await execText('git', ['push', remoteName, `HEAD:${branchName}`], repoRoot);
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

export async function waitForGreenCi(repoRoot, config, sha) {
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

async function finalizeIteration(repoRoot, config, context) {
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

  while (true) {
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

    if (!(await hasCommitCandidateChanges(repoRoot, config.initialUntracked, config.initialTrackedPaths))) {
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
  if (reportNeedsAttention(uiReport)) {
    if (!config.uiFixCommand) {
      throw new Error(`UI audit did not pass. Configure uiFixCommand or inspect ${context.ui_report_file}.`);
    }

    logStep(`Iteration ${iteration}: UI fixes`);
    await runShellCommand(config.uiFixCommand, repoRoot, context, {
      stepName: 'UI fixes',
    });

    uiReport = await generateUiReport(repoRoot, context, config);
    if (reportNeedsAttention(uiReport)) {
      throw new Error(`UI audit still does not pass after uiFixCommand. See ${context.ui_report_file}.`);
    }
  }

  logStep(`Iteration ${iteration}: formal verification`);
  let formalReport = await generateFormalReport(repoRoot, context, config);
  if (reportNeedsAttention(formalReport)) {
    if (!config.formalFixCommand) {
      throw new Error(
        `Formal verification did not pass. Configure formalFixCommand or inspect ${context.formal_report_file}.`,
      );
    }

    logStep(`Iteration ${iteration}: formal fixes`);
    await runShellCommand(config.formalFixCommand, repoRoot, context, {
      stepName: 'Formal fixes',
    });

    formalReport = await generateFormalReport(repoRoot, context, config);
    if (reportNeedsAttention(formalReport)) {
      throw new Error(`Formal verification still does not pass after formalFixCommand. See ${context.formal_report_file}.`);
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

  const startState = await ensureStartState(repoRoot, config.allowDirty);
  config.initialTrackedPaths = startState.initialTrackedPaths;
  config.initialUntracked = startState.initialUntracked;
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

function isCliEntry() {
  if (!process.argv[1]) {
    return false;
  }

  return import.meta.url === pathToFileURL(path.resolve(process.argv[1])).href;
}

if (isCliEntry()) {
  main().catch((error) => {
    console.error(error.message);
    process.exit(1);
  });
}
