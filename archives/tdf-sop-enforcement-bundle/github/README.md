
# GitHub Actions — YouTube Upload

- Workflow: `.github/workflows/upload-youtube.yml` (usa runner self-hosted).
- Scripts: `github/scripts/youtube_upload.py`, `github/scripts/notify_discord.py`.
- Secrets necesarios: `YOUTUBE_CLIENT_ID`, `YOUTUBE_CLIENT_SECRET`, `YOUTUBE_REFRESH_TOKEN`, `DISCORD_WEBHOOK_URL` (opcional),
  `BACKEND_CALLBACK_URL` y `BACKEND_TOKEN` (opcional).

Desencadenado por `repository_dispatch` con payload que contiene:
- `video_file_id`, `thumb_file_id`, `title`, `description`, `tags`, `playlist_id`, `publish_at`.
