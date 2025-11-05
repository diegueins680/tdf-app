
import argparse, json, os, requests
p = argparse.ArgumentParser()
p.add_argument("--status", required=True)
p.add_argument("--video-url", default="")
p.add_argument("--context", default="")
a = p.parse_args()

url = os.environ.get("DISCORD_WEBHOOK_URL")
if not url:
    print("No DISCORD_WEBHOOK_URL set"); exit(0)

content = {
  "content": f"**YouTube Upload {a.status}**\nContexto: {a.context}\n{('Video: ' + a.video_url) if a.video_url else ''}"
}
try:
    requests.post(url, json=content, timeout=30)
except Exception as e:
    print("Discord notify failed:", e)
