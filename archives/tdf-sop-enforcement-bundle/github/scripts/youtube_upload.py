
import argparse, json, os, time, sys
from google.oauth2.credentials import Credentials
from googleapiclient.discovery import build
from googleapiclient.http import MediaFileUpload
from googleapiclient.errors import HttpError

def get_service():
    creds = Credentials.from_authorized_user_info({
        "client_id": os.environ["YT_CLIENT_ID"],
        "client_secret": os.environ["YT_CLIENT_SECRET"],
        "refresh_token": os.environ["YT_REFRESH_TOKEN"]
    })
    return build("youtube", "v3", credentials=creds, cache_discovery=False)

def main():
    p = argparse.ArgumentParser()
    p.add_argument("--file", required=True)
    p.add_argument("--title", required=True)
    p.add_argument("--description", required=True)
    p.add_argument("--tags", default="")
    p.add_argument("--categoryId", default="10")
    p.add_argument("--language", default="es")
    p.add_argument("--privacyStatus", default="private")
    p.add_argument("--publishAt", required=True)  # ISO8601 con zona
    p.add_argument("--playlistId", required=True)
    p.add_argument("--thumbnail", default="")
    p.add_argument("--madeForKids", default="false")
    p.add_argument("--out", default="yt_out.json")
    args = p.parse_args()

    tags = [t.strip() for t in args.tags.split(",") if t.strip()]
    body = {
        "snippet": {
            "title": args.title,
            "description": args.description,
            "tags": tags,
            "categoryId": args.categoryId,
            "defaultLanguage": args.language,
            "defaultAudioLanguage": args.language
        },
        "status": {
            "privacyStatus": args.privacyStatus,
            "publishAt": args.publishAt,
            "selfDeclaredMadeForKids": (args.madeForKids.lower() == "true")
        }
    }

    yt = get_service()
    media = MediaFileUpload(args.file, chunksize=8*1024*1024, resumable=True)
    request = yt.videos().insert(part="snippet,status", body=body, media_body=media)
    response = None
    while response is None:
        try:
            status, response = request.next_chunk()
            if status:
                print(f"Uploaded {int(status.progress()*100)}%")
        except HttpError as e:
            if e.resp.status in [500, 502, 503, 504]:
                time.sleep(5); continue
            print("Upload failed:", e, file=sys.stderr)
            sys.exit(2)

    video_id = response["id"]
    video_url = f"https://www.youtube.com/watch?v={video_id}"

    if args.thumbnail and os.path.exists(args.thumbnail):
        yt.thumbnails().set(videoId=video_id, media_body=args.thumbnail).execute()

    yt.playlistItems().insert(part="snippet", body={
        "snippet": {"playlistId": args.playlistId, "resourceId": {"kind":"youtube#video","videoId": video_id}}
    }).execute()

    with open(args.out,"w") as f:
        json.dump({"videoId": video_id, "videoUrl": video_url}, f)
    print(video_url)

if __name__ == "__main__":
    main()
