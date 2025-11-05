
YouTube OAuth — Cómo obtener REFRESH TOKEN (para GitHub Actions)

1) Crea un OAuth Client ID en Google Cloud (tipo: Desktop App).
2) En tu máquina, instala dependencias:
   pip install google-auth google-auth-oauthlib google-auth-httplib2
3) Ejecuta el script:
   python yt_get_refresh_token.py
4) Ingresa tu CLIENT_ID y CLIENT_SECRET cuando lo solicite.
5) Abre la URL que muestra la consola, autoriza, copia el código y pégalo en la consola.
6) El script imprimirá el REFRESH TOKEN.
7) Guarda estos secrets en el repo del workflow:
   - YOUTUBE_CLIENT_ID
   - YOUTUBE_CLIENT_SECRET
   - YOUTUBE_REFRESH_TOKEN
