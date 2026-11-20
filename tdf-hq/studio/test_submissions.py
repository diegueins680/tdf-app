import base64
import io
import os
from pathlib import Path
import sqlite3
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

    def test_limits_and_invalid_files(self):
        with self.assertRaises(Problem):
            self.upload(30)
        self.path.write_bytes(b"invalid")
        with self.assertRaises(OSError):
            self.upload()
        self.assertEqual(self.call("queue"), [])


if __name__ == "__main__":
    unittest.main()
