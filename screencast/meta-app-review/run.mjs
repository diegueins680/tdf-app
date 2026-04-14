import { mkdir } from 'node:fs/promises';
import { chromium } from 'playwright';

const URL = process.env.TDF_REVIEW_URL ?? 'https://tdf-app.pages.dev/social/instagram?review=1';
const INBOX_URL = process.env.TDF_REVIEW_INBOX_URL ?? 'https://tdf-app.pages.dev/social/inbox?review=1';
const OUT_DIR = process.env.TDF_SCREENCAST_OUT_DIR ?? 'screencast/meta-app-review/output';
const REVIEW_USER = process.env.TDF_REVIEW_USERNAME ?? 'admin';
const REVIEW_PASS = process.env.TDF_REVIEW_PASSWORD ?? 'password123';
const ENABLE_SPOTLIGHT = process.env.TDF_REVIEW_SPOTLIGHT !== '0';
const SPOTLIGHT_MS_RAW = Number.parseInt(process.env.TDF_REVIEW_SPOTLIGHT_MS ?? '1200', 10);
const SPOTLIGHT_MS = Number.isFinite(SPOTLIGHT_MS_RAW) && SPOTLIGHT_MS_RAW > 0 ? SPOTLIGHT_MS_RAW : 1200;
const INBOUND_TEXT_OVERRIDE = process.env.TDF_REVIEW_INBOUND_TEXT?.trim() ?? '';

const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
const setupHeadingMatcher = /Meta App Review:\s*(Facebook|Instagram) Login/i;
const inboxHeadingMatcher = /Meta App Review:\s*Messaging Inbox/i;

/**
 * Human-in-the-loop pauses:
 * - Meta login / consent can require MFA/captcha.
 * - We pause and ask the operator to complete it, then continue.
 */
async function pauseForHuman(page, message) {
  console.log('\n=== ACTION REQUIRED ===');
  console.log(message);
  console.log('Press ENTER in this terminal to continue...');
  await page.bringToFront();
  await new Promise((resolve) => process.stdin.once('data', resolve));
}

async function spotlight(page, locator, label, durationMs = SPOTLIGHT_MS) {
  if (!ENABLE_SPOTLIGHT) {
    return;
  }
  const target = locator.first();
  const visible = await target.isVisible({ timeout: 3000 }).catch(() => false);
  if (!visible) {
    return;
  }
  await target.scrollIntoViewIfNeeded().catch(() => {});
  const handle = await target.elementHandle();
  if (!handle) {
    return;
  }

  await handle
    .evaluate(
      (el, payload) => {
        const rect = el.getBoundingClientRect();
        if (!rect || rect.width <= 0 || rect.height <= 0) {
          return;
        }

        const existing = document.getElementById('tdf-review-spotlight-overlay');
        if (existing) {
          existing.remove();
        }

        const overlay = document.createElement('div');
        overlay.id = 'tdf-review-spotlight-overlay';
        overlay.style.position = 'fixed';
        overlay.style.left = `${Math.max(0, rect.left - 8)}px`;
        overlay.style.top = `${Math.max(0, rect.top - 8)}px`;
        overlay.style.width = `${Math.min(window.innerWidth, rect.width + 16)}px`;
        overlay.style.height = `${Math.min(window.innerHeight, rect.height + 16)}px`;
        overlay.style.border = '3px solid #00d4ff';
        overlay.style.borderRadius = '12px';
        overlay.style.boxShadow = '0 0 0 9999px rgba(5,12,30,0.18), 0 0 22px rgba(0,212,255,0.75)';
        overlay.style.pointerEvents = 'none';
        overlay.style.zIndex = '2147483647';
        overlay.style.opacity = '1';
        overlay.style.transition = 'transform 150ms ease-out, opacity 220ms ease-out';
        overlay.style.transform = 'scale(1)';

        if (payload.labelText) {
          const chip = document.createElement('div');
          chip.textContent = payload.labelText;
          chip.style.position = 'absolute';
          chip.style.left = '8px';
          chip.style.top = '-34px';
          chip.style.padding = '6px 10px';
          chip.style.borderRadius = '999px';
          chip.style.background = 'rgba(0, 22, 33, 0.9)';
          chip.style.color = '#d6f7ff';
          chip.style.fontFamily = 'Inter, ui-sans-serif, system-ui, -apple-system, Segoe UI, Roboto, sans-serif';
          chip.style.fontSize = '12px';
          chip.style.fontWeight = '600';
          chip.style.whiteSpace = 'nowrap';
          chip.style.letterSpacing = '0.02em';
          overlay.appendChild(chip);
        }

        document.body.appendChild(overlay);
        requestAnimationFrame(() => {
          overlay.style.transform = 'scale(1.02)';
        });
        setTimeout(() => {
          overlay.style.opacity = '0';
          overlay.style.transform = 'scale(1.03)';
          setTimeout(() => overlay.remove(), 260);
        }, Math.max(250, payload.durationMs));
      },
      { labelText: label, durationMs }
    )
    .catch(() => {});

  await sleep(durationMs + 180);
  await handle.dispose().catch(() => {});
}

const iso = () => new Date().toISOString().replace(/[:.]/g, '-');

async function ensureLoggedIn(page) {
  const onLoginPath = /\/login(?:\?|$)/.test(page.url());
  const loginHeading = page.getByRole('heading', { name: /Iniciar sesi[oó]n|Sign in|Login/i });
  const onLoginScreen = onLoginPath || (await loginHeading.count()) > 0;

  if (!onLoginScreen) {
    return;
  }

  console.log('Login page detected. Signing in with review credentials...');

  const username = page.getByRole('textbox', { name: /Usuario o correo|Username|Email/i }).first();
  const password = page.getByRole('textbox', { name: /Contrase[nñ]a|Password/i }).first();
  const submit = page.getByRole('button', { name: /Ingresar|Log in|Sign in/i }).first();

  await username.waitFor();
  await username.fill(REVIEW_USER);
  await password.fill(REVIEW_PASS);
  await submit.click();

  await Promise.race([
    page.waitForURL((u) => !/\/login(?:\?|$)/.test(u.pathname), { timeout: 90_000 }),
    page.getByRole('button', { name: /Salir|Logout|Log out/i }).first().waitFor({ timeout: 90_000 }),
  ]);

  if (/\/login(?:\?|$)/.test(page.url())) {
    throw new Error(`Login did not leave /login. Current URL: ${page.url()}`);
  }

  await page.waitForLoadState('networkidle').catch(() => {});
}

async function gotoReviewPage(page, url) {
  await page.goto(url, { waitUntil: 'domcontentloaded' });
  await page.waitForLoadState('networkidle', { timeout: 15_000 }).catch(() => {});
}

const normalizeText = (value) => value.replace(/\s+/g, ' ').trim();

function rowLooksFailed(text) {
  return /delivery blocked|message delivery failed|failed/i.test(text);
}

async function findPreferredInboxConversationRow(page, preferredInboundText) {
  const refreshBtn = page.getByRole('button', { name: /Refresh/i }).first();
  const allFilterBtn = page.getByRole('button', { name: /^All$/i }).first();
  const instagramTable = page.locator('table').first();
  const instagramRows = instagramTable.locator('tbody tr');

  const findVisibleConversationRow = async () => {
    const count = await instagramRows.count();
    let fallbackRow = null;
    let cleanRow = null;
    for (let index = 0; index < count; index += 1) {
      const row = instagramRows.nth(index);
      const text = normalizeText((await row.textContent()) ?? '');
      if (!text) {
        continue;
      }
      if (/No messages for this filter|Sin mensajes para este filtro/i.test(text)) {
        continue;
      }
      if (!fallbackRow) {
        fallbackRow = row;
      }
      if (preferredInboundText && text.toLowerCase().includes(preferredInboundText.toLowerCase())) {
        return row;
      }
      if (!cleanRow && !rowLooksFailed(text)) {
        cleanRow = row;
      }
    }
    return cleanRow ?? fallbackRow;
  };

  for (let attempt = 0; attempt < 4; attempt += 1) {
    await instagramTable.waitFor({ state: 'visible', timeout: 15_000 }).catch(() => {});
    const row = await findVisibleConversationRow();
    if (row) {
      return row;
    }

    if (attempt === 0) {
      const refreshVisible = await refreshBtn.isVisible({ timeout: 2_000 }).catch(() => false);
      if (refreshVisible) {
        await refreshBtn.click().catch(() => {});
      }
    } else if (attempt === 1) {
      const allVisible = await allFilterBtn.isVisible({ timeout: 2_000 }).catch(() => false);
      if (allVisible) {
        await allFilterBtn.click().catch(() => {});
      }
      const refreshVisible = await refreshBtn.isVisible({ timeout: 2_000 }).catch(() => false);
      if (refreshVisible) {
        await refreshBtn.click().catch(() => {});
      }
    }

    await page.waitForLoadState('networkidle').catch(() => {});
    await sleep(2_000);
  }

  const tableText = normalizeText((await instagramTable.textContent().catch(() => '')) ?? '');
  throw new Error(`No inbound Instagram conversation row became visible. Instagram table text: ${tableText || '(empty)'}`);
}

const main = async () => {
  await mkdir(OUT_DIR, { recursive: true });

  const context = await chromium.launchPersistentContext('', {
    headless: false,
    locale: 'en-US',
    // Best-effort to keep UI in English. (The app itself must also render English in review mode.)
    args: ['--lang=en-US', '--disable-features=TranslateUI'],
    recordVideo: { dir: OUT_DIR, size: { width: 1920, height: 1080 } },
    viewport: { width: 1920, height: 1080 },
  });

  const page = await context.newPage();
  page.setDefaultTimeout(90_000);

  console.log(`Opening: ${URL}`);
  await gotoReviewPage(page, URL);
  await ensureLoggedIn(page);

  // After login we may land elsewhere; always return to review setup page.
  await gotoReviewPage(page, URL);
  await ensureLoggedIn(page);

  // Step 1: Instagram setup screen (review mode)
  await page.getByRole('heading', { name: setupHeadingMatcher }).waitFor();
  console.log('Reached review setup screen.');

  // Ensure the Asset selection panel is visible (Meta reviewer requirement)
  await page
    .getByRole('heading', { name: /Asset selection/i })
    .scrollIntoViewIfNeeded({ timeout: 5_000 })
    .catch(() => {});

  const continueBtn = page.getByRole('button', { name: /Continue to message send flow/i });
  const continueAlreadyVisible = await continueBtn.first().isVisible({ timeout: 3_000 }).catch(() => false);
  console.log(`Continue button visible before auth step: ${continueAlreadyVisible}`);

  // Click connect / re-authorize to show Meta login + permissions modal
  const connectBtn = page.getByRole('button', { name: /Connect with Meta Login|Re-authorize/i });
  const hasConnectBtn = await connectBtn.first().isVisible({ timeout: 5_000 }).catch(() => false);
  console.log(`Connect button visible: ${hasConnectBtn}`);

  if (!continueAlreadyVisible && hasConnectBtn) {
    await page.getByRole('heading', { name: /Connection status/i }).scrollIntoViewIfNeeded().catch(() => {});
    await spotlight(page, connectBtn, 'Connect with Meta');
    await connectBtn.first().click();
    console.log('Connect/Re-authorize clicked. Waiting for manual Meta consent step.');

    await pauseForHuman(
    page,
    'Complete the full Meta login flow + grant permissions in the browser window.\n' +
        '- Keep the permissions dialog visible for a moment (Meta guideline).\n' +
        '- Finish consent and return to the app tab with the selected professional/business account visible.\n'
    );

    // After human completes auth, return to setup page.
    await gotoReviewPage(page, URL).catch(() => {});
    await ensureLoggedIn(page);
    await page.getByRole('heading', { name: setupHeadingMatcher }).waitFor();
    console.log('Returned from Meta consent to setup page.');
  }

  // Make sure the Messaging asset dropdown is visible and has a selected value
  const assetSelect = page.getByLabel(/Messaging asset/i);
  await assetSelect.scrollIntoViewIfNeeded().catch(() => {});

  // Step 2: Proceed to inbox review mode
  const hasContinueBtn = await continueBtn.first().isVisible({ timeout: 8_000 }).catch(() => false);
  if (!hasContinueBtn) {
    await pauseForHuman(
      page,
      'The Continue button is not visible yet.\n' +
        '- Confirm Meta connection completed successfully.\n' +
        '- Select a Messaging asset (Page + IG account) if required.\n' +
        '- Ensure "Continue to message send flow" is visible.\n'
    );
  }
  const continueVisibleAfterPrompt = await continueBtn.first().isVisible({ timeout: 5_000 }).catch(() => false);
  if (continueVisibleAfterPrompt) {
    await spotlight(page, continueBtn, 'Continue to Inbox Flow');
    await continueBtn.click();
    console.log('Navigating to messaging inbox flow via Continue button.');
  } else {
    console.log('Continue button still not visible; navigating directly to inbox review URL.');
    await gotoReviewPage(page, INBOX_URL);
    await ensureLoggedIn(page);
  }

  await page.getByRole('heading', { name: inboxHeadingMatcher }).waitFor();
  console.log('Reached review messaging inbox screen.');

  const inboundMarker =
    INBOUND_TEXT_OVERRIDE && INBOUND_TEXT_OVERRIDE.length > 0
      ? INBOUND_TEXT_OVERRIDE
      : `Meta review inbound marker ${iso()} - from IG native client`;

  // We need an inbound message to reply to. If none, operator should send one to the connected asset.
  await pauseForHuman(
    page,
    'Ensure there is a CLEAN INBOUND Instagram message visible in the Inbox list from the selected professional/business account.\n' +
      'If Meta needs a fresh proof thread, use the exact inbound text below.\n' +
      `Preferred inbound text for this run:\n${inboundMarker}\n\n` +
      'Open Instagram and send that exact message to the connected professional/business account.\n' +
      'Wait until that new row appears here, and avoid reusing an older failed thread if a clean new one is available.\n'
  );

  await page.waitForLoadState('networkidle').catch(() => {});
  await sleep(1_500);

  // Prefer the Instagram panel and wait for a real message row instead of a
  // transient empty/loading state.
  const firstRow = await findPreferredInboxConversationRow(page, inboundMarker);
  await spotlight(page, firstRow, 'Open inbound conversation');
  await firstRow.click();

  // Dialog opens: type reply, send
  const outgoing = page.getByLabel(/Outgoing message/i);
  await outgoing.waitFor();
  await spotlight(page, outgoing, 'Compose outgoing message');
  const unique = `Meta review test message ${iso()} - sent from TDF HQ UI`;
  console.log(`Prepared outbound verification message: ${unique}`);
  await outgoing.fill(unique);

  const sendBtn = page.getByRole('button', { name: /^Send message$/i });
  await spotlight(page, sendBtn, 'Send message');
  await sendBtn.click();
  console.log('Send message clicked. Checking confirmation UI...');

  // Confirmation UI can vary by locale/version; try a few known signals.
  let sendConfirmationDetected = false;
  try {
    await Promise.race([
      page.getByText(/Step 3 of 3:|Paso 3 de 3:/i).first().waitFor({ timeout: 20_000 }),
      page
        .getByText(/sent successfully|message sent|mensaje enviado|delivery confirmation/i)
        .first()
        .waitFor({ timeout: 20_000 }),
    ]);
    sendConfirmationDetected = true;
  } catch {
    // Fall through to manual verification prompt.
  }
  console.log(`Send confirmation detected in UI: ${sendConfirmationDetected}`);

  await pauseForHuman(
    page,
    'Now switch to the native Instagram client (Android) and capture the same thread.\n' +
      `Message to verify first:\n${unique}\n\n` +
      'In that native-client segment, show the delivered message, then delete or unsend that same message.\n' +
      'Return to TDF HQ, keep the review checklist visible, and wait for the inbox auto-refresh to reflect the deletion without a manual reload.\n' +
      'Press ENTER only after both the native-client delete/unsend and the in-app deleted-message refresh are captured.\n'
  );

  // Give a little tail for editing once the deleted-message refresh has been captured.
  await sleep(2500);

  await context.close();
  console.log('Done. Video saved under:', OUT_DIR);
  console.log('Verification message:', unique);
};

main().catch((err) => {
  console.error(err);
  process.exit(1);
});
