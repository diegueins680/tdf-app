# Meta App Review Screencast (Instagram)

This folder automates the **desktop** portion of the Meta App Review screencast:
- Meta login + consent (human-in-the-loop)
- Visible asset selection
- Live send from app UI
- Automatic spotlight highlights on key UI actions (connect, continue, compose, send)
- Produces a browser recording (Playwright) you can post-process and stitch with the Android native-client clip.

Permissions in scope:
- `instagram_basic`
- `instagram_manage_messages`
- `instagram_business_basic`
- `instagram_business_manage_messages`

Provider mapping in this app:
- `facebook` provider run: `instagram_basic`, `instagram_manage_messages`
- `instagram` provider run: `instagram_business_basic`, `instagram_business_manage_messages`

## 1) Run the desktop screencast

From repo root:

```bash
node screencast/meta-app-review/run.mjs
```

Notes:
- The script will **pause** and ask you to complete Meta login/consent if needed.
- It will also pause to ensure an inbound message exists.
- If App Review requires all four permissions, record two runs (one per provider config) and submit both clips or a stitched final video.
- Set `TDF_REVIEW_SPOTLIGHT=0` to disable on-screen highlights.
- Set `TDF_REVIEW_SPOTLIGHT_MS=1500` to control highlight duration in milliseconds.

Output video is saved under:

```
screencast/meta-app-review/output/
```

## 2) Record the Android native-client segment

Record a short clip showing:
- The same thread
- The exact message sent from the app UI appearing in the Instagram Android client

## 3) Stitch / render

```bash
./screencast/meta-app-review/render.sh \
  --desktop screencast/meta-app-review/output/<desktop>.webm \
  --phone /path/to/android.mp4 \
  --captions /path/to/review-captions.srt \
  --narration-text /path/to/narration.en.txt \
  --voice Samantha \
  --out screencast/meta-app-review/output/final.mp4
```

If you don't have the phone clip yet, omit `--phone`.
If you already recorded narration audio, use `--narration-audio /path/to/voiceover.wav` instead of `--narration-text`.
If you want automatic captions (Whisper CLI), use `--autocaptions` instead of `--captions`.

## 4) Quick Narration File (optional)

Create a plain text file (English), for example:

```text
This video demonstrates the Instagram messaging flow used for Meta App Review.
First, we connect the app with Meta Login and grant permissions.
Then we open an inbound thread and send a reply from the TDF HQ interface.
Finally, we verify the exact same message in the Instagram native client.
```

Then pass it with `--narration-text`.
