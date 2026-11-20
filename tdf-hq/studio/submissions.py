"""Studio intake on the application's private persistent uploads mount.

Each invocation is one API operation. SQLite transactions keep photo bytes, their
review state and retry receipts together, including across process restarts.
"""

import base64
import hashlib
import hmac
import fcntl
import resource
import io
import json
import os
from pathlib import Path
import secrets
import sqlite3
import subprocess
import sys
import tempfile
import time
import uuid
import warnings

from PIL import Image, ImageOps

MAX_FILE = 25 * 1024**2
MAX_STORAGE = 5 * 1024**3
DAY = 86400
Image.MAX_IMAGE_PIXELS = 50_000_000


class Problem(Exception):
    def __init__(self, status, message):
        self.status = status
        super().__init__(message)


def require(condition, status, message):
    if not condition:
        raise Problem(status, message)


def derivatives(path):
    with tempfile.TemporaryDirectory() as directory:
        source = path
        with open(path, "rb") as stream:
            header = stream.read(32)
        if sys.platform.startswith("linux"):
            resource.setrlimit(resource.RLIMIT_AS, (1024**3, 1024**3))
        if header[4:8] == b"ftyp" and header[8:12] in (
            b"heic",
            b"heix",
            b"hevc",
            b"hevx",
            b"mif1",
            b"msf1",
        ):
            source = str(Path(directory) / "primary.png")
            subprocess.run(
                ["heif-thumbnailer", "-p", "-s", "1600", path, source],
                timeout=30,
                check=True,
                stdout=subprocess.DEVNULL,
                stderr=subprocess.DEVNULL,
            )
        with warnings.catch_warnings(), Image.open(
            source, formats=["JPEG", "PNG", "WEBP"]
        ) as image:
            warnings.simplefilter("error", Image.DecompressionBombWarning)
            require(
                image.width * image.height <= Image.MAX_IMAGE_PIXELS,
                422,
                "La imagen es demasiado grande.",
            )
            image = ImageOps.exif_transpose(image).convert("RGBA")
            flattened = Image.new("RGB", image.size, "white")
            flattened.paste(image, mask=image.getchannel("A"))
            result = []
            for width in (1600, 640):
                flattened.thumbnail((width, width), Image.Resampling.LANCZOS)
                output = io.BytesIO()
                flattened.save(output, "JPEG", quality=84, optimize=True)
                result.append(output.getvalue())
            return result


def initialize(db):
    db.executescript("""
        CREATE TABLE IF NOT EXISTS settings (id INTEGER PRIMARY KEY CHECK(id=1), token TEXT);
        INSERT OR IGNORE INTO settings VALUES (1, NULL);
        CREATE TABLE IF NOT EXISTS batches (
          id INTEGER PRIMARY KEY, receipt TEXT UNIQUE NOT NULL, created REAL NOT NULL, credit TEXT NOT NULL);
        CREATE TABLE IF NOT EXISTS photos (
          id TEXT PRIMARY KEY, batch INTEGER NOT NULL REFERENCES batches(id), position INTEGER NOT NULL,
          name TEXT NOT NULL, state TEXT NOT NULL DEFAULT 'pending', changed REAL NOT NULL,
          digest TEXT NOT NULL, original BLOB, large BLOB, small BLOB,
          UNIQUE(batch,position), UNIQUE(batch,digest));
        CREATE TABLE IF NOT EXISTS attempts (at REAL NOT NULL);
        CREATE INDEX IF NOT EXISTS attempt_time ON attempts(at);
    """)


def cleanup(db, now):
    db.execute("DELETE FROM attempts WHERE at < ?", (now - 3600,))
    db.execute(
        """UPDATE photos SET state=CASE WHEN state='pending' THEN 'expired' ELSE state END,original=NULL,large=NULL,small=NULL
      WHERE state IN ('pending','rejected') AND changed < ? AND original IS NOT NULL""",
        (now - 30 * DAY,),
    )
    db.execute(
        """DELETE FROM photos WHERE state IN ('rejected','expired') AND changed < ?""",
        (now - 90 * DAY,),
    )
    db.execute(
        "DELETE FROM batches WHERE created < ? AND id NOT IN (SELECT batch FROM photos)",
        (now - DAY,),
    )


def dispatch(db, op, data):
    now = time.time()
    if op not in ("status", "queue", "link", "preview", "image", "authorize"):
        db.execute("BEGIN IMMEDIATE")
    token = db.execute("SELECT token FROM settings").fetchone()[0]
    if op == "cleanup":
        cleanup(db, now)
        return {}
    if op in ("link", "replace", "disable"):
        if op != "link":
            token = secrets.token_urlsafe(32) if op == "replace" else None
            db.execute("UPDATE settings SET token=?", (token,))
        return {"token": token}
    if op in ("batch", "upload", "authorize"):
        require(
            token and hmac.compare_digest(token.encode(), str(data.get("token", "")).encode()),
            403,
            "El enlace está desactivado.",
        )
        if op == "authorize":
            return {}
        receipt = str(uuid.UUID(data["receipt"]))
        batch = db.execute("SELECT id FROM batches WHERE receipt=?", (receipt,)).fetchone()
        if op == "batch":
            if not batch:
                require(
                    db.execute(
                        "SELECT count(*) FROM batches WHERE created>?", (now - 3600,)
                    ).fetchone()[0]
                    < 100,
                    429,
                    "Demasiados lotes. Intenta más tarde.",
                )
                db.execute(
                    "INSERT INTO batches(receipt,created,credit) VALUES(?,?,?)",
                    (receipt, now, str(data.get("credit", ""))[:250]),
                )
            return {}
        require(batch, 404, "Lote desconocido.")
        position = data["position"]
        require(isinstance(position, int) and 0 <= position < 30, 400, "Máximo 30 fotos.")
        path = data["path"]
        size = os.path.getsize(path)
        require(0 < size <= MAX_FILE, 413, "Máximo 25 MB por foto.")
        original = Path(path).read_bytes()
        digest = hashlib.sha256(original).hexdigest()
        previous = db.execute(
            "SELECT position,digest FROM photos WHERE batch=? AND (position=? OR digest=?)",
            (batch[0], position, digest),
        ).fetchone()
        if previous:
            require(previous[1] == digest, 409, "Esta posición ya contiene otra foto.")
            return {"duplicate": previous[0] != position}
        require(
            db.execute("SELECT count(*) FROM attempts WHERE at>?", (now - 3600,)).fetchone()[0]
            < 240,
            429,
            "Límite temporal de fotos alcanzado.",
        )
        db.execute("INSERT INTO attempts VALUES(?)", (now,))
        db.commit()
        # Reserve one decoder at a time across API processes before decoding.
        with open(data["lock"], "a") as lock:
            try:
                fcntl.flock(lock, fcntl.LOCK_EX | fcntl.LOCK_NB)
            except BlockingIOError:
                raise Problem(429, "El estudio procesa otra foto. Reintenta en un momento.")
            large, small = derivatives(path)
            db.execute("BEGIN IMMEDIATE")
            current = db.execute("SELECT token FROM settings").fetchone()[0]
            require(current == token, 403, "El enlace ha cambiado.")
            used = db.execute(
                "SELECT coalesce(sum(length(original)+length(large)+length(small)),0) FROM photos"
            ).fetchone()[0]
            require(
                used + size + len(large) + len(small) <= MAX_STORAGE, 409, "Almacenamiento lleno."
            )
            db.execute(
                """INSERT OR IGNORE INTO photos(id,batch,position,name,changed,digest,original,large,small)
                VALUES(?,?,?,?,?,?,?,?,?)""",
                (
                    str(uuid.uuid4()),
                    batch[0],
                    position,
                    str(data["name"])[:200],
                    now,
                    digest,
                    original,
                    large,
                    small,
                ),
            )
            stored = db.execute(
                "SELECT position FROM photos WHERE batch=? AND digest=?", (batch[0], digest)
            ).fetchone()
            require(stored, 409, "La posición ya contiene otra foto.")
            return {"duplicate": stored[0] != position}
    if op == "status":
        receipt = str(uuid.UUID(data["receipt"]))
        return [
            dict(row)
            for row in db.execute(
                """SELECT position,name,state FROM photos
            JOIN batches ON batches.id=photos.batch WHERE receipt=? ORDER BY position""",
                (receipt,),
            )
        ]
    if op == "queue":
        return [
            dict(row)
            for row in db.execute(
                """SELECT photos.id,batch,position,name,state,credit FROM photos
            JOIN batches ON batches.id=photos.batch WHERE state IN ('pending','accepted') ORDER BY batch DESC,position"""
            )
        ]
    if op == "review":
        ids = data["ids"]
        require(isinstance(ids, list) and 0 < len(ids) <= 30, 400, "Selección inválida.")
        state = data["state"]
        require(state in ("accepted", "rejected"), 400, "Decisión inválida.")
        for identifier in ids:
            cursor = db.execute(
                """UPDATE photos SET state=?,changed=? WHERE id=? AND
                (state='pending' OR state=?)""",
                (state, now, str(uuid.UUID(identifier)), state),
            )
            require(cursor.rowcount == 1, 409, "La revisión ya cambió.")
        return {}
    if op in ("preview", "image"):
        column = "large" if data.get("size") == "large" else "small"
        row = db.execute(
            f"SELECT {column},state FROM photos WHERE id=?", (str(uuid.UUID(data["id"])),)
        ).fetchone()
        require(
            row and row[0] and (row[1] == "accepted" or (op == "preview" and row[1] == "pending")),
            404,
            "Foto no disponible.",
        )
        return {"jpeg": base64.b64encode(row[0]).decode("ascii")}
    raise Problem(400, "Operación desconocida.")


def main():
    os.umask(0o077)
    root = Path("uploads/studio-submissions")
    root.mkdir(parents=True, exist_ok=True)
    db = sqlite3.connect(root / "intake.sqlite", timeout=5)
    db.row_factory = sqlite3.Row
    db.execute("PRAGMA foreign_keys=ON")
    db.execute("PRAGMA secure_delete=ON")
    if db.execute("PRAGMA user_version").fetchone()[0] == 0:
        initialize(db)
        db.execute("PRAGMA user_version=1")
    data = json.load(sys.stdin)
    require(isinstance(data, dict), 400, "Solicitud inválida.")
    data["lock"] = str(root / "decode.lock")
    with db:
        value = dispatch(db, sys.argv[1], data)
    print(json.dumps({"ok": value}))


if __name__ == "__main__":
    try:
        main()
    except Problem as error:
        print(json.dumps({"status": error.status, "error": str(error)}))
    except (
        ValueError,
        KeyError,
        TypeError,
        OSError,
        subprocess.SubprocessError,
        Image.DecompressionBombError,
        Image.DecompressionBombWarning,
    ):
        print(json.dumps({"status": 422, "error": "No se pudo leer la foto o la solicitud."}))
