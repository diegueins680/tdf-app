#!/usr/bin/env python3
"""
YouTube OAuth (console flow) -> obtiene un REFRESH TOKEN para usar en GitHub Actions.
Prerrequisitos:
  pip install google-auth google-auth-oauthlib google-auth-httplib2
Uso:
  1) Ejecuta:  python yt_get_refresh_token.py
  2) Pega tu CLIENT_ID y CLIENT_SECRET cuando lo pida.
  3) Copia/pega la URL en un navegador, autoriza y pega el código de verificación en la consola.
  4) El script imprimirá el REFRESH_TOKEN que debes guardar como secret.
"""
import json
from google_auth_oauthlib.flow import InstalledAppFlow

SCOPES = ["https://www.googleapis.com/auth/youtube.upload"]

def main():
    client_id = input("CLIENT_ID: ").strip()
    client_secret = input("CLIENT_SECRET: ").strip()

    client_config = {
        "installed": {
            "client_id": client_id,
            "client_secret": client_secret,
            "project_id": "tdf-youtube-uploader",
            "auth_uri": "https://accounts.google.com/o/oauth2/auth",
            "token_uri": "https://oauth2.googleapis.com/token",
            "redirect_uris": ["urn:ietf:wg:oauth:2.0:oob", "http://localhost"]
        }
    }

    flow = InstalledAppFlow.from_client_config(client_config, SCOPES)
    creds = flow.run_console()
    print("\n=== COPIA ESTE REFRESH TOKEN ===")
    print(creds.refresh_token)
    print("================================\n")

if __name__ == "__main__":
    main()
