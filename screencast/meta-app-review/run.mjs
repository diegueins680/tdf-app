import { chromium } from 'playwright';

const URL = process.env.TDF_REVIEW_URL ?? 'https://tdf-app.pages.dev/social/instagram?review=1';
const OUT_DIR = process.env.TDF_SCREENCAST_OUT_DIR ?? 'screencast/meta-app-review/output';

const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

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

const main = async () => {
  const context = await chromium.launchPersistentContext('', {
    headless: false,
    locale: 'en-US',
    // Best-effort to keep UI in English. (The app itself must also render English in review mode.)
    args: ['--lang=en-US', '--disable-features=TranslateUI'],
    recordVideo: { dir: OUT_DIR, size: { width: 1920, height: 1080 } },
    viewport: { width: 1920, height: 1080 },
  });

  const page = await context.newPage();
  page.setDefaultTimeout(45_000);

  console.log(`Opening: ${URL}`);
  await page.goto(URL, { waitUntil: 'networkidle' });

  // Step 1: Instagram setup screen (review mode)
  await page.getByRole('heading', { name: /Meta App Review: Instagram Setup/i }).waitFor();

  // Ensure the Asset selection panel is visible (Meta reviewer requirement)
  await page.getByRole('heading', { name: /Asset selection/i }).scrollIntoViewIfNeeded().catch(() => {});

  // Click connect / re-authorize to show Meta login + permissions modal
  const connectBtn = page.getByRole('button', { name: /Connect with Meta Login|Re-authorize/i });
  if (await connectBtn.count()) {
    await page.getByRole('heading', { name: /Connection status/i }).scrollIntoViewIfNeeded().catch(() => {});
    await connectBtn.first().click();

    await pauseForHuman(
      page,
      'Complete the full Meta login flow + grant permissions in the browser window.\n' +
        '- Keep the permissions dialog visible for a moment (Meta guideline).\n' +
        '- Finish consent and return to the app tab.\n'
    );

    // After human completes auth, return to setup page (the app often redirects back automatically).
    await page.goto(URL, { waitUntil: 'networkidle' }).catch(() => {});
    await page.getByRole('heading', { name: /Meta App Review: Instagram Setup/i }).waitFor();
  }

  // Make sure the Messaging asset dropdown is visible and has a selected value
  const assetSelect = page.getByLabel(/Messaging asset/i);
  await assetSelect.scrollIntoViewIfNeeded().catch(() => {});

  // Step 2: Proceed to inbox review mode
  const continueBtn = page.getByRole('button', { name: /Continue to message send flow/i });
  await continueBtn.click();

  await page.getByRole('heading', { name: /Meta App Review: Messaging Inbox/i }).waitFor();

  // We need an inbound message to reply to. If none, operator should send one to the connected asset.
  await pauseForHuman(
    page,
    'Ensure there is an INBOUND Instagram message visible in the Inbox list (from @0iego.saa).\n' +
      'If none, open Instagram and send a message to the business account so it appears here.\n'
  );

  // Open first message row by clicking a sender cell (best-effort)
  const firstRow = page.locator('table tbody tr').first();
  await firstRow.waitFor();
  await firstRow.click();

  // Dialog opens: type reply, send
  const outgoing = page.getByLabel(/Outgoing message/i);
  await outgoing.waitFor();
  const unique = `Meta review test message ${iso()} — sent from TDF HQ UI`;
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
};

main().catch((err) => {
  console.error(err);
  process.exit(1);
});
