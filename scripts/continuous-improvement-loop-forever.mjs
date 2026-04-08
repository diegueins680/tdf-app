#!/usr/bin/env node

import fs from 'node:fs/promises';
import { createWriteStream } from 'node:fs';
import path from 'node:path';
import process from 'node:process';
import { execFileSync, spawn } from 'node:child_process';
import { checkpointDirtyWorktree } from './lib/continuous-improvement-loop-dirty-worktree.mjs';

const ROOT = process.cwd();
const STATE_DIR = path.join(ROOT, 'tmp', 'continuous-improvement-loop');
const CYCLES_DIR = path.join(STATE_DIR, 'cycles');
const STATUS_FILE = path.join(STATE_DIR, 'status.json');
const PID_FILE = path.join(STATE_DIR, 'runner.pid');
const STOP_FILE = path.join(STATE_DIR, 'stop');
const HEARTBEAT_FILE = path.join(STATE_DIR, 'heartbeat.txt');
const RUNNER_LOG_FILE = path.join(STATE_DIR, 'runner.log');
const DEFAULT_CONFIG = path.join(ROOT, 'scripts', 'continuous-improvement-loop.codex.json');
const CONFIG_PATH = path.resolve(process.argv[2] ?? process.env.CONTINUOUS_LOOP_CONFIG ?? DEFAULT_CONFIG);
const LOOP_INTERVAL_SECONDS = clampInt(process.env.CONTINUOUS_LOOP_FOREVER_INTERVAL_SECONDS, 300, 15, 86400);
const STOP_POLL_SECONDS = clampInt(process.env.CONTINUOUS_LOOP_FOREVER_STOP_POLL_SECONDS, 5, 1, 60);
const CYCLE_TIMEOUT_SECONDS = clampInt(process.env.CONTINUOUS_LOOP_CYCLE_TIMEOUT_SECONDS, 7200, 60, 86400);
const CYCLE_KILL_GRACE_SECONDS = clampInt(process.env.CONTINUOUS_LOOP_CYCLE_KILL_GRACE_SECONDS, 30, 5, 300);
const CHILD_ARGS = [
  'scripts/continuous-improvement-loop.mjs',
  '--config',
  CONFIG_PATH,
  '--max-iterations',
  '1',
];

let runnerState = {
  mode: 'forever',
  pid: process.pid,
  state: 'starting',
  root: ROOT,
  configPath: relativePath(CONFIG_PATH),
  intervalSeconds: LOOP_INTERVAL_SECONDS,
  stopPollSeconds: STOP_POLL_SECONDS,
  cycleTimeoutSeconds: CYCLE_TIMEOUT_SECONDS,
  cycleKillGraceSeconds: CYCLE_KILL_GRACE_SECONDS,
  startedAt: new Date().toISOString(),
  updatedAt: new Date().toISOString(),
  heartbeatAt: new Date().toISOString(),
  cycleCount: 0,
  pidFile: relativePath(PID_FILE),
  stopFile: relativePath(STOP_FILE),
  heartbeatFile: relativePath(HEARTBEAT_FILE),
  runnerLogFile: relativePath(RUNNER_LOG_FILE),
  statusFile: relativePath(STATUS_FILE),
  stateDir: relativePath(STATE_DIR),
  childArgs: CHILD_ARGS.map((value) => (value === ROOT ? '.' : value)),
};

let shutdownRequest = null;
let activeChild = null;

async function main() {
  await ensureStateDir();
  await ensureSingleRunner();
  await clearStopRequest();
  await fs.writeFile(PID_FILE, `${process.pid}\n`, 'utf8');
  installSignalHandlers();
  await logRunner(`started persistent runner with config=${relativePath(CONFIG_PATH)} interval=${LOOP_INTERVAL_SECONDS}s timeout=${CYCLE_TIMEOUT_SECONDS}s`);
  await updateRunnerStatus({ state: 'idle', nextRunAt: null, blockReason: null, blockDetails: null });

  try {
    while (true) {
      if (await syncStopFileState()) break;

      const block = await detectPreflightBlock();
      if (block) {
        await logRunner(`preflight blocked: ${block.reason}`);
        await updateRunnerStatus({
          state: 'blocked',
          blockReason: block.reason,
          blockDetails: block.details,
          nextRunAt: futureIso(LOOP_INTERVAL_SECONDS),
          currentLogFile: null,
        });
        if (await sleepWithStopPolling(LOOP_INTERVAL_SECONDS)) break;
        continue;
      }

      const cycleIndex = runnerState.cycleCount + 1;
      const cycleStamp = new Date().toISOString().replace(/[:.]/g, '-');
      const cycleLogFile = path.join(CYCLES_DIR, `cycle-${String(cycleIndex).padStart(4, '0')}-${cycleStamp}.log`);

      await updateRunnerStatus({
        state: 'running',
        cycleCount: cycleIndex,
        blockReason: null,
        blockDetails: null,
        currentLogFile: relativePath(cycleLogFile),
        nextRunAt: null,
      });
      await logRunner(`cycle ${cycleIndex} starting -> ${relativePath(cycleLogFile)}`);

      const result = await runBoundedCycle(cycleIndex, cycleLogFile);
      await logRunner(
        `cycle ${cycleIndex} finished code=${result.exitCode ?? 'null'} signal=${result.signal ?? 'none'} timedOut=${String(result.timedOut)}`,
      );

      await updateRunnerStatus({
        state: shutdownRequest ? 'stopping' : 'sleeping',
        lastCycle: {
          index: cycleIndex,
          startedAt: result.startedAt,
          endedAt: result.endedAt,
          exitCode: result.exitCode,
          signal: result.signal,
          timedOut: result.timedOut,
          logFile: relativePath(cycleLogFile),
        },
        nextRunAt: shutdownRequest ? null : futureIso(LOOP_INTERVAL_SECONDS),
      });

      if (await syncStopFileState()) break;
      if (await sleepWithStopPolling(LOOP_INTERVAL_SECONDS)) break;
    }
  } finally {
    const shutdown = shutdownRequest ?? {
      reason: 'completed',
      requestedAt: new Date().toISOString(),
    };

    await updateRunnerStatus({
      state: 'stopped',
      nextRunAt: null,
      shutdown,
      finishedAt: new Date().toISOString(),
      currentLogFile: null,
    }).catch(() => {});
    await logRunner(`stopped (${shutdown.reason})`).catch(() => {});
    await safeUnlink(PID_FILE);
  }
}

function clampInt(raw, fallback, min, max) {
  const number = Number(raw);
  if (!Number.isFinite(number)) return fallback;
  return Math.min(max, Math.max(min, Math.trunc(number)));
}

function relativePath(target) {
  return path.relative(ROOT, target) || '.';
}

function futureIso(seconds) {
  return new Date(Date.now() + seconds * 1000).toISOString();
}

async function ensureStateDir() {
  await fs.mkdir(CYCLES_DIR, { recursive: true });
}

async function ensureSingleRunner() {
  const rawPid = await fs.readFile(PID_FILE, 'utf8').catch(() => '');
  const pid = Number.parseInt(rawPid.trim(), 10);
  if (!Number.isFinite(pid) || pid <= 0) return;

  try {
    process.kill(pid, 0);
    throw new Error(`Continuous improvement loop runner is already active with PID ${pid}.`);
  } catch (error) {
    if (error && typeof error === 'object' && 'code' in error && error.code === 'ESRCH') {
      return;
    }
    if (error instanceof Error) throw error;
    throw new Error(String(error));
  }
}

async function clearStopRequest() {
  await safeUnlink(STOP_FILE);
}

async function updateRunnerStatus(patch) {
  const heartbeatAt = new Date().toISOString();
  runnerState = {
    ...runnerState,
    ...patch,
    updatedAt: heartbeatAt,
    heartbeatAt,
  };
  await fs.writeFile(STATUS_FILE, `${JSON.stringify(runnerState, null, 2)}\n`, 'utf8');
  await fs.writeFile(HEARTBEAT_FILE, `${heartbeatAt}\n`, 'utf8');
}

async function logRunner(message) {
  await fs.appendFile(RUNNER_LOG_FILE, `[${new Date().toISOString()}] ${message}\n`, 'utf8');
}

async function detectPreflightBlock() {
  let config;
  try {
    config = JSON.parse(await fs.readFile(CONFIG_PATH, 'utf8'));
  } catch (error) {
    return {
      reason: `config unavailable at ${relativePath(CONFIG_PATH)}`,
      details: [error instanceof Error ? error.message : String(error)],
    };
  }

  if (configReferencesCodex(config) && !commandExists('codex')) {
    return {
      reason: 'Codex CLI is required by the configured continuous-improvement-loop worker but is unavailable',
      details: [relativePath(CONFIG_PATH)],
    };
  }

  if (config.pollGitHub !== false && !githubCredentialsAvailable()) {
    return {
      reason: 'GitHub polling is enabled but neither token env vars nor `gh auth token` are available',
      details: [relativePath(CONFIG_PATH), 'Set GITHUB_TOKEN/GH_TOKEN/GITHUB_PAT or authenticate `gh auth login`.'],
    };
  }

  let dirtyLines = gitStatusLines();
  if (dirtyLines.length > 0) {
    const dirtyCheckpoint = await checkpointDirtyWorktree({ repoRoot: ROOT });
    dirtyLines = gitStatusLines();
    if (dirtyLines.length === 0 && dirtyCheckpoint?.recovered) {
      await logRunner(dirtyCheckpoint.summary || 'checkpointed dirty worktree before continuing');
      return null;
    }
    return {
      reason:
        dirtyCheckpoint?.recovered
          ? 'dirty worktree persisted after auto-checkpoint recovery; waiting before the next bounded cycle'
          : dirtyCheckpoint?.reason || 'dirty worktree; waiting for operator cleanup before forever loop cycles run',
      details: dirtyLines.slice(0, 40),
    };
  }

  return null;
}

function configReferencesCodex(config) {
  const commandKeys = ['ideaCommand', 'implementationCommand', 'uiFixCommand', 'formalFixCommand', 'ciRepairCommand'];
  return commandKeys.some((key) => /(^|\b)codex(\b|[-/])|codex-loop-worker/.test(String(config?.[key] ?? '')));
}

function commandExists(command) {
  try {
    execFileSync('/bin/bash', ['-lc', `command -v ${shellQuote(command)} >/dev/null`], {
      cwd: ROOT,
      stdio: ['ignore', 'ignore', 'ignore'],
    });
    return true;
  } catch {
    return false;
  }
}

function githubCredentialsAvailable() {
  if (process.env.GITHUB_TOKEN || process.env.GH_TOKEN || process.env.GITHUB_PAT) {
    return true;
  }

  try {
    const token = execFileSync('gh', ['auth', 'token'], {
      cwd: ROOT,
      encoding: 'utf8',
      stdio: ['ignore', 'pipe', 'ignore'],
    }).trim();
    return token.length > 0;
  } catch {
    return false;
  }
}

function gitStatusLines() {
  try {
    const output = execFileSync('git', ['status', '--porcelain'], {
      cwd: ROOT,
      encoding: 'utf8',
      stdio: ['ignore', 'pipe', 'pipe'],
    });
    return output
      .split(/\r?\n/)
      .map((line) => line.trimEnd())
      .filter(Boolean);
  } catch (error) {
    return [error instanceof Error ? error.message : String(error)];
  }
}

function shellQuote(value) {
  return `'${String(value).replace(/'/g, `'\\''`)}'`;
}

async function runBoundedCycle(cycleIndex, cycleLogFile) {
  const startedAt = new Date().toISOString();
  const logStream = createWriteStream(cycleLogFile, { flags: 'a' });
  logStream.write(`[${startedAt}] cycle ${cycleIndex} starting with ${JSON.stringify(CHILD_ARGS)}\n`);

  return await new Promise((resolve, reject) => {
    const child = spawn(process.execPath, CHILD_ARGS, {
      cwd: ROOT,
      env: {
        ...process.env,
        CONTINUOUS_LOOP_FOREVER_MODE: '1',
      },
      stdio: ['ignore', 'pipe', 'pipe'],
    });

    activeChild = { child, cycleIndex };
    let timedOut = false;

    const timeoutHandle = setTimeout(() => {
      timedOut = true;
      logStream.write(`[${new Date().toISOString()}] cycle ${cycleIndex} exceeded ${CYCLE_TIMEOUT_SECONDS}s; sending SIGTERM\n`);
      child.kill('SIGTERM');
      setTimeout(() => {
        if (!activeChild || activeChild.child.pid !== child.pid) return;
        logStream.write(`[${new Date().toISOString()}] cycle ${cycleIndex} still active after ${CYCLE_KILL_GRACE_SECONDS}s grace; sending SIGKILL\n`);
        child.kill('SIGKILL');
      }, CYCLE_KILL_GRACE_SECONDS * 1000).unref();
    }, CYCLE_TIMEOUT_SECONDS * 1000);

    const stopPollHandle = setInterval(() => {
      void syncStopFileState();
    }, STOP_POLL_SECONDS * 1000);

    child.stdout.on('data', (chunk) => {
      logStream.write(chunk);
    });
    child.stderr.on('data', (chunk) => {
      logStream.write(chunk);
    });

    child.on('error', (error) => {
      clearTimeout(timeoutHandle);
      clearInterval(stopPollHandle);
      activeChild = null;
      logStream.end();
      reject(error);
    });

    child.on('close', (exitCode, signal) => {
      clearTimeout(timeoutHandle);
      clearInterval(stopPollHandle);
      activeChild = null;
      const endedAt = new Date().toISOString();
      logStream.write(
        `[${endedAt}] cycle ${cycleIndex} finished code=${exitCode ?? 'null'} signal=${signal ?? 'none'} timedOut=${String(timedOut)}\n`,
      );
      logStream.end();
      resolve({ startedAt, endedAt, exitCode, signal, timedOut });
    });
  });
}

async function stopFileExists() {
  try {
    await fs.access(STOP_FILE);
    return true;
  } catch {
    return false;
  }
}

async function syncStopFileState() {
  if (!(await stopFileExists())) return Boolean(shutdownRequest);
  requestShutdown('stop file requested by operator');
  return true;
}

function requestShutdown(reason, signalName = 'SIGTERM') {
  if (shutdownRequest) return;

  shutdownRequest = {
    reason,
    signal: signalName,
    requestedAt: new Date().toISOString(),
  };

  void fs.writeFile(STOP_FILE, `${shutdownRequest.requestedAt} ${reason}\n`, 'utf8');
  void updateRunnerStatus({ state: 'stopping', shutdown: shutdownRequest, nextRunAt: null });
  void logRunner(`shutdown requested (${reason})`);

  if (!activeChild) return;
  activeChild.child.kill(signalName);
  setTimeout(() => {
    if (!activeChild) return;
    activeChild.child.kill('SIGKILL');
  }, CYCLE_KILL_GRACE_SECONDS * 1000).unref();
}

function installSignalHandlers() {
  process.on('SIGINT', () => {
    requestShutdown('received SIGINT', 'SIGINT');
  });
  process.on('SIGTERM', () => {
    requestShutdown('received SIGTERM', 'SIGTERM');
  });
}

async function sleepWithStopPolling(seconds) {
  const deadline = Date.now() + seconds * 1000;
  while (Date.now() < deadline) {
    if (await syncStopFileState()) return true;
    const remainingMs = deadline - Date.now();
    if (remainingMs <= 0) break;
    await wait(Math.min(remainingMs, STOP_POLL_SECONDS * 1000));
  }
  return await syncStopFileState();
}

function wait(milliseconds) {
  return new Promise((resolve) => {
    setTimeout(resolve, milliseconds);
  });
}

async function safeUnlink(filePath) {
  await fs.unlink(filePath).catch(() => {});
}

main().catch(async (error) => {
  const message = error instanceof Error ? error.stack || error.message : String(error);
  await updateRunnerStatus({
    state: 'error',
    nextRunAt: null,
    error: message,
    finishedAt: new Date().toISOString(),
  }).catch(() => {});
  await logRunner(`fatal error: ${message}`).catch(() => {});
  console.error(message);
  process.exitCode = 1;
});
