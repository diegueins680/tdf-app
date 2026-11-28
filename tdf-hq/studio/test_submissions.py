import base64
import io
import os
from pathlib import Path
import sqlite3
import subprocess
import sys
import tempfile
import time
import unittest
import uuid

from PIL import Image
from submissions import DAY, Problem, dispatch, initialize


class StudioIntakeTest(unittest.TestCase):
    def setUp(self):
        directory = tempfile.TemporaryDirectory()
        self.addCleanup(directory.cleanup)
        root = Path(directory.name)
        self.db = sqlite3.connect(root / "store.sqlite")
        self.db.row_factory = sqlite3.Row
        self.addCleanup(self.db.close)
        initialize(self.db)
        self.path = root / "camera.jpg"
        exif = Image.Exif()
        exif[274] = 6
        exif[315] = "private metadata"
        Image.new("RGB", (1800, 900), "teal").save(self.path, exif=exif)
        self.lock = os.fspath(root / "lock")
        self.token = self.call("replace")["token"]
        self.receipt = str(uuid.uuid4())
        self.call("batch", token=self.token, receipt=self.receipt, credit="TDF")

    def call(self, op, **args):
        with self.db:
            return dispatch(self.db, op, dict(args, lock=self.lock))

    def upload(self, pos=0):
        return self.call(
            "upload",
            token=self.token,
            receipt=self.receipt,
            position=pos,
            path=self.path,
            name=self.path.name,
        )

    def test_review_retry_privacy_and_derivatives(self):
        self.assertFalse(self.upload()["duplicate"])
        self.assertFalse(self.upload()["duplicate"])
        self.assertTrue(self.upload(1)["duplicate"])
        (row,) = self.call("queue")
        self.assertEqual(row["credit"], "TDF")
        with self.assertRaises(Problem):
            self.call("image", id=row["id"])
        self.assertEqual(self.call("status", receipt=str(uuid.uuid4())), [])
        self.call("review", ids=[row["id"]], state="accepted")
        image = Image.open(
            io.BytesIO(base64.b64decode(self.call("image", id=row["id"], size="large")["jpeg"]))
        )
        self.assertEqual(image.size, (800, 1600))
        self.assertFalse(image.getexif())
        self.assertEqual(
            self.db.execute("SELECT original FROM photos").fetchone()[0], self.path.read_bytes()
        )

    def test_rotation_and_disable(self):
        self.call("replace")
        with self.assertRaises(Problem):
            self.upload()
        self.token = self.call("link")["token"]
        self.upload()
        self.call("disable")
        with self.assertRaises(Problem):
            self.upload()

    def test_rejection_retention_and_atomic_bulk_review(self):
        self.upload()
        (row,) = self.call("queue")
        with self.assertRaises(Problem):
            self.call("review", ids=[row["id"], str(uuid.uuid4())], state="accepted")
        self.assertEqual(self.call("queue")[0]["state"], "pending")
        self.call("review", ids=[row["id"]], state="rejected")
        with self.assertRaises(Problem):
            self.call("preview", id=row["id"])
        with self.db:
            self.db.execute("UPDATE photos SET changed=?", (time.time() - 31 * DAY,))
        self.call("cleanup")
        self.assertIsNone(self.db.execute("SELECT original FROM photos").fetchone()[0])

    def test_webp_and_png_are_decoded_by_content(self):
        for position, image_format in enumerate(("WEBP", "PNG")):
            Image.new("RGB", (96, 64), "plum").save(self.path, format=image_format)
            self.assertFalse(self.upload(position)["duplicate"])
        self.assertEqual(len(self.call("queue")), 2)

    def test_large_photos_stay_within_the_container_memory(self):
        # A 50-megapixel camera JPEG is decoded at reduced scale and accepted.
        exif = Image.Exif()
        exif[274] = 6
        Image.new("RGB", (8660, 5773), "teal").save(self.path, exif=exif)
        script = (
            "import resource, sys; sys.path.insert(0, sys.argv[1]); import submissions; "
            "large, small = submissions.derivatives(sys.argv[2]); "
            "peak = resource.getrusage(resource.RUSAGE_SELF).ru_maxrss; "
            "print(peak // (1024 if sys.platform == 'darwin' else 1))"
        )
        peak_kib = int(
            subprocess.run(
                [sys.executable, "-c", script, os.path.dirname(os.path.abspath(__file__)), self.path],
                check=True,
                capture_output=True,
                text=True,
            ).stdout
        )
        self.assertLess(peak_kib, 256 * 1024)
        self.assertFalse(self.upload()["duplicate"])
        (row,) = self.call("queue")
        self.call("review", ids=[row["id"]], state="accepted")
        image = Image.open(
            io.BytesIO(base64.b64decode(self.call("image", id=row["id"], size="large")["jpeg"]))
        )
        self.assertEqual(image.size, (1067, 1600))
        # Formats that cannot be decoded at reduced scale are refused above the budget.
        for position, (image_format, size) in enumerate(
            (("PNG", (4001, 3250)), ("WEBP", (4000, 2001))), start=1
        ):
            Image.new("RGB", size, "plum").save(self.path, format=image_format)
            with self.assertRaises(Problem) as problem:
                self.upload(position)
            self.assertEqual(problem.exception.status, 422)
        self.assertEqual(len(self.call("queue")), 1)

    def test_rate_and_file_size_limits(self):
        with open(self.path, "wb") as stream:
            stream.truncate(25 * 1024**2 + 1)
        with self.assertRaises(Problem) as problem:
            self.upload()
        self.assertEqual(problem.exception.status, 413)
        Image.new("RGB", (2, 2), "blue").save(self.path)
        with self.db:
            self.db.executemany("INSERT INTO attempts VALUES(?)", [(time.time(),)] * 240)
        with self.assertRaises(Problem) as problem:
            self.upload()
        self.assertEqual(problem.exception.status, 429)

    def test_limits_and_invalid_files(self):
        with self.assertRaises(Problem):
            self.upload(30)
        self.path.write_bytes(b"invalid")
        with self.assertRaises(OSError):
            self.upload()
        self.assertEqual(self.call("queue"), [])


if __name__ == "__main__":
    unittest.main()
