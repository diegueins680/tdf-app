import { mkdir } from 'node:fs/promises';
import { chromium } from 'playwright';

const URL = process.env.TDF_REVIEW_URL ?? 'https://tdf-app.pages.dev/social/instagram?review=1';
const INBOX_URL = process.env.TDF_REVIEW_INBOX_URL ?? 'https://tdf-app.pages.dev/social/inbox?review=1';
const OUT_DIR = process.env.TDF_SCREENCAST_OUT_DIR ?? 'screencast/meta-app-review/output';
const REVIEW_USER = process.env.TDF_REVIEW_USERNAME ?? 'admin';
const REVIEW_PASS = process.env.TDF_REVIEW_PASSWORD ?? 'password123';

const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
const setupHeadingMatcher = /Meta App Review:\s*Instagram Setup/i;
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
  await page.goto(URL, { waitUntil: 'networkidle' });
  await ensureLoggedIn(page);

  // After login we may land elsewhere; always return to review setup page.
  await page.goto(URL, { waitUntil: 'networkidle' });
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
    await connectBtn.first().click();
    console.log('Connect/Re-authorize clicked. Waiting for manual Meta consent step.');

    await pauseForHuman(
      page,
      'Complete the full Meta login flow + grant permissions in the browser window.\n' +
        '- Keep the permissions dialog visible for a moment (Meta guideline).\n' +
        '- Finish consent and return to the app tab.\n'
    );

    // After human completes auth, return to setup page.
    await page.goto(URL, { waitUntil: 'networkidle' }).catch(() => {});
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
    await continueBtn.click();
    console.log('Navigating to messaging inbox flow via Continue button.');
  } else {
    console.log('Continue button still not visible; navigating directly to inbox review URL.');
    await page.goto(INBOX_URL, { waitUntil: 'networkidle' });
    await ensureLoggedIn(page);
  }

  await page.getByRole('heading', { name: inboxHeadingMatcher }).waitFor();
  console.log('Reached review messaging inbox screen.');

  // We need an inbound message to reply to. If none, operator should send one to the connected asset.
  await pauseForHuman(
    page,
    'Ensure there is an INBOUND Instagram message visible in the Inbox list (from @0iego.saa).\n' +
      'If none, open Instagram and send a message to the business account so it appears here.\n'
  );

  // Open first message row by clicking a sender cell.
  const firstRow = page.locator('table tbody tr').first();
  if ((await firstRow.count()) === 0) {
    throw new Error('No inbound conversation rows found in the inbox table.');
  }
  await firstRow.click();

  // Dialog opens: type reply, send
  const outgoing = page.getByLabel(/Outgoing message/i);
  await outgoing.waitFor();
  const unique = `Meta review test message ${iso()} - sent from TDF HQ UI`;
  console.log(`Prepared outbound verification message: ${unique}`);
  await outgoing.fill(unique);

  const sendBtn = page.getByRole('button', { name: /^Send message$/i });
  await sendBtn.click();

  // Confirmation panel: show exact text to verify in native client
  await page.getByText(/Step 3 of 3:/i).waitFor();

  await pauseForHuman(
    page,
    'Now switch to the native Instagram client (Android) and verify the SAME message appears in the thread.\n' +
      `Message to verify:\n${unique}\n\n` +
      'Record that phone segment separately; we will stitch it in post.\n'
  );

  // Give a little tail for editing
  await sleep(1500);

  await context.close();
  console.log('Done. Video saved under:', OUT_DIR);
  console.log('Verification message:', unique);
};

main().catch((err) => {
  console.error(err);
  process.exit(1);
});
