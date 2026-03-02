# Meta App Review Screencast (Instagram)

This folder automates the **desktop** portion of the Meta App Review screencast:
- Meta login + consent (human-in-the-loop)
- Visible asset selection
- Live send from app UI
- Produces a browser recording (Playwright) you can post-process and stitch with the Android native-client clip.

## 1) Run the desktop screencast

From repo root:

```bash
node screencast/meta-app-review/run.mjs
```

Notes:
- The script will **pause** and ask you to complete Meta login/consent if needed.
- It will also pause to ensure an inbound message exists.

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
  --out screencast/meta-app-review/output/final.mp4
```

If you don't have the phone clip yet, omit `--phone`.

## TODO (next iteration)
- Add TTS narration generation (English)
- Add captions burn-in (SRT via Whisper)
- Add automatic zoom/highlight around key UI elements
