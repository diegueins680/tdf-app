import base64
import contextlib
import hashlib
import json
import os
import signal
import subprocess as sp
import sys
import tempfile
import time
import urllib.request
import urllib.error
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
PORT = 18979
API = f"http://127.0.0.1:{PORT}"
PROFILE = "/directory/profiles/tdf-records-estudio"
DB = "studio_recovery_test"
CANDIDATE = ROOT / "executable/studio-test-candidate/tdf-hq-exe"
PREVIOUS = ROOT / "executable/studio-test-previous/tdf-hq-exe"
FIXTURES = ROOT / "e2e/web/fixtures/studio-feed"
EXPECTED = [hashlib.sha256((FIXTURES / f"{i}.png").read_bytes()).hexdigest() for i in range(1, 7)]


def run(*args, **kwargs):
    return sp.run(args, check=True, **kwargs)


def db_url(port):
    return f"postgresql://postgres:synthetic@127.0.0.1:{port}/{DB}"


def get(path):
    with urllib.request.urlopen(API + path, timeout=8) as response:
        assert response.status == 200
        return json.load(response)


def inspect():
    posts = get(PROFILE)["feed"]
    assert len(posts) == 1
    post = posts[0]
    assert post["caption"] == "editado" and len(set(post["images"])) == 6
    hashes = []
    for image in post["images"]:
        prefix, data = get(PROFILE + "/feed/" + image).split(",", 1)
        assert prefix == "data:image/png;base64"
        hashes.append(hashlib.sha256(base64.b64decode(data, validate=True)).hexdigest())
    assert hashes == EXPECTED
    return {"post": post, "anonymous_sha256_in_order": hashes}


def wait_health(process):
    for _ in range(75):
        if process.poll() is not None:
            raise RuntimeError("Backend exited with " + str(process.returncode))
        try:
            if get("/health")["db"] == "ok":
                return
        except OSError:
            pass
        time.sleep(1)
    raise RuntimeError("Backend never became healthy")


@contextlib.contextmanager
def backend(binary, port, data, working):
    working.mkdir()
    mount = working / "uploads"
    mount.mkdir()
    assets = working / "assets"
    assets.mkdir()
    run("sudo", "mount", "--bind", data, mount)
    try:
        assert sp.check_output(["findmnt", "--noheadings", "--mountpoint", mount, "--output", "TARGET"], text=True).strip() == str(mount)
        assert os.path.samefile(mount, data)
        env = dict(os.environ, CONTENT_STORAGE_UPLOADS_ENABLED="true", HQ_ASSETS_DIR=str(assets),
                   HQ_ASSETS_BASE_URL=API + "/assets/serve", DATABASE_URL=db_url(port),
                   APP_PORT=str(PORT), RUN_MIGRATIONS="false", AUTO_APPLY_PRODUCTION_MIGRATIONS="false",
                   RESET_DB="false", SEED_DB="false", DEFAULT_LOCALE="es", EVENT_DISCOVERY_ENABLED="false")
        with (working / "api.log").open("w") as log:
            proc = sp.Popen([binary], cwd=working, env=env, stdout=log, stderr=log)
            try:
                wait_health(proc)
                yield proc.pid
            finally:
                if proc.poll() is None:
                    proc.terminate()
                    proc.wait(timeout=30)
    finally:
        run("sudo", "umount", mount)


def digest(path):
    with path.open("rb") as stream:
        return hashlib.file_digest(stream, "sha256").hexdigest()


def manifest(root):
    return {str(p.relative_to(root)): digest(p) for p in sorted(root.rglob("*")) if p.is_file()}


def setup_db(name, port):
    run("docker", "run", "--detach", "--name", name, "-e", "POSTGRES_PASSWORD=synthetic",
        "-e", "POSTGRES_DB=" + DB, "-p", f"127.0.0.1:{port}:5432", "pgvector/pgvector:pg17")
    for _ in range(80):
        if sp.run(["psql", db_url(port), "-XAtqc", "SELECT 1"], capture_output=True).returncode == 0:
            return
        time.sleep(1)
    raise RuntimeError("Disposable PostgreSQL unavailable")


def hook(work, server_pid):
    # The unmodified HTTP harness has just completed withdrawal, republication and profile pause.
    # Restore only this disposable fixture's listability for the recovery checkpoint.
    run("psql", db_url(19327), "-X", "-v", "ON_ERROR_STOP=1", "-c",
        "UPDATE directory_profile SET profile_status='published' WHERE id='a0000000-0000-4000-8000-000000000003'")
    first = inspect()
    os.kill(server_pid, signal.SIGTERM)
    for _ in range(120):
        if not Path(f"/proc/{server_pid}").exists():
            break
        time.sleep(.25)
    else:
        raise RuntimeError("Original process still alive")
    with tempfile.TemporaryDirectory() as root:
        scratch = Path(root)
        private = work / "uploads"
        with backend(CANDIDATE, 19327, private, scratch / "replacement") as replacement:
            assert replacement != server_pid
            assert inspect() == first
        # Both API processes are stopped. No external peers can reach this private fixture.
        peer_count = sp.check_output(["psql", db_url(19327), "-XAtqc",
            "SELECT count(*) FROM pg_stat_activity WHERE datname=current_database() AND pid<>pg_backend_pid() AND backend_type='client backend'"]).strip()
        assert peer_count == b"0"
        files = manifest(private)
        for image, expected in zip(first["post"]["images"], EXPECTED):
            assert files["content-storage/" + image] == expected
        dump = scratch / "database.dump"
        with dump.open("wb") as out:
            run("docker", "exec", "studio-source", "pg_dump", "-U", "postgres", "-Fc", DB, stdout=out)
        archive = scratch / "uploads.tar"
        run("tar", "-cf", archive, "-C", private, ".")
        archive_hashes = [digest(dump), digest(archive)]
        # Stop the source DB and make its file tree unavailable under the original path.
        run("docker", "stop", "studio-source")
        private.rename(work / "offline-source")
        setup_db("studio-restored", 19328)
        with dump.open("rb") as inp:
            run("docker", "exec", "-i", "studio-restored", "pg_restore", "--exit-on-error",
                "-U", "postgres", "-d", DB, stdin=inp)
        restored = scratch / "restored-volume"
        restored.mkdir()
        run("tar", "-xf", archive, "-C", restored)
        assert manifest(restored) == files and [digest(dump), digest(archive)] == archive_hashes
        with backend(CANDIDATE, 19328, restored, scratch / "restored-candidate"):
            assert inspect() == first
        with backend(PREVIOUS, 19328, restored, scratch / "previous"):
            previous = get(PROFILE)
            assert previous["slug"] == "tdf-records-estudio" and "feed" not in previous
        with backend(CANDIDATE, 19328, restored, scratch / "candidate-again"):
            assert inspect() == first
        result = dict(first, binaries={"candidate": digest(CANDIDATE), "previous": digest(PREVIOUS)},
                      archive_sha256=dict(postgres=digest(dump), uploads=digest(archive)),
                      private_file_count=len(files), source_stopped=True, fresh_database_container=True,
                      checks=["initial-anonymous", "replacement-process-same-bind", "restored-anonymous",
                              "previous-backend-public-profile", "candidate-after-previous"])
        # Exercise the real maintenance response and a directly addressed origin.
        # Keep the historical executable stopped throughout the hold.
        import runpy
        import socket
        launch = runpy.run_path(str(ROOT / "ops/hetzner/guarded-release.py"))
        config = scratch / "Caddyfile"
        config.write_bytes(launch["MAINTENANCE_CADDYFILE"].replace(b"api.tdfrecords.net {", b":18981 {"))
        run("docker", "run", "--detach", "--name", "studio-maintenance", "--network", "host",
            "--mount", f"type=bind,src={config},dst=/etc/caddy/Caddyfile,readonly", "caddy:2")
        def edge(method, path):
            req = urllib.request.Request("http://127.0.0.1:18981" + path, method=method,
                      headers={"Authorization":"Bearer synthetic-artist-listing-token"})
            try:
                with urllib.request.urlopen(req, timeout=3) as response:
                    return response.status, response.headers.get("X-TDF-Maintenance")
            except urllib.error.HTTPError as e:
                return e.code, e.headers.get("X-TDF-Maintenance")
        def closed(port):
            with socket.socket() as sock:
                sock.settimeout(2)
                import errno
                return sock.connect_ex(("127.0.0.1", port)) == errno.ECONNREFUSED
        for _ in range(30):
            try:
                if edge("GET","/health") == (503, "tdf-release-maintenance"):
                    break
            except OSError:
                pass
            time.sleep(1)
        image_path = "/storage/files/" + first["post"]["images"][0]
        cases = [("GET","/health"),("DELETE",image_path),("DELETE","/api"+image_path),
                 ("POST","/drive/upload"),("PUT","/storage/me/destination"),
                 ("PUT","/directory/profiles/a0000000-0000-4000-8000-000000000003/feed/"+first["post"]["id"]),
                 ("PATCH","/directory/profiles/a0000000-0000-4000-8000-000000000003/feed/"+first["post"]["id"])]
        for method, resource in cases:
            assert edge(method, resource) == (503,"tdf-release-maintenance")
        with backend(CANDIDATE, 19328, restored, scratch / "edge-only-negative-control"):
            assert not closed(PORT) and get("/health")["db"] == "ok"
            assert edge("DELETE", image_path)[0] == 503
        run("docker","stop","studio-restored")
        assert closed(PORT) and closed(19328)
        assert manifest(restored) == files
        for method, resource in cases:
            assert edge(method, resource) == (503,"tdf-release-maintenance")
        run("docker","start","studio-restored")
        for _ in range(45):
            if sp.run(["psql",db_url(19328),"-XAtqc","SELECT 1"],capture_output=True).returncode==0:
                break
            time.sleep(1)
        with backend(CANDIDATE,19328,restored,scratch/"verified-forward-recovery"):
            assert inspect() == first
            assert edge("GET","/health")[0] == 503
        result["maintenance_proof"] = {"edge_status":503, "tested_requests":cases,
            "edge_alone_direct_health":200,"origin_during_hold":"connection refused",
            "database_during_hold":"connection refused","private_manifest_preserved":True,
            "six_hashes_verified_after_forward_recovery":True}
        # Normal owner action on the restored synthetic copy only, after the clean round trip.
        image = first["post"]["images"][0]
        def owner_delete():
            request = urllib.request.Request(API + "/storage/files/" + image, method="DELETE",
                      headers={"Authorization": "Bearer synthetic-artist-listing-token"})
            try:
                with urllib.request.urlopen(request, timeout=8) as response:
                    return response.status
            except urllib.error.HTTPError as error:
                return error.code
        with backend(CANDIDATE, 19328, restored, scratch / "current-delete-check"):
            current_code = owner_delete()
            assert current_code == 409
            assert inspect() == first
        with backend(PREVIOUS, 19328, restored, scratch / "previous-delete-check"):
            previous_code = owner_delete()
            assert previous_code in (204, 409)
        with backend(CANDIDATE, 19328, restored, scratch / "after-delete-check"):
            try:
                get(PROFILE + "/feed/" + image)
                read_code = 200
            except urllib.error.HTTPError as error:
                read_code = error.code
            assert (previous_code, read_code) in ((204, 404), (409, 200))
        result["owner_storage_delete_on_restored_copy"] = {
            "candidate_status": current_code, "previous_status": previous_code,
            "anonymous_image_after_previous": read_code}
        evidence = json.dumps(result, ensure_ascii=False, indent=2)
        (ROOT / "studio-feed-recovery-result.json").write_text(evidence + "\n")
        print(evidence, flush=True)


def main():
    setup_db("studio-source", 19327)
    # Insert a temporary callback into the established harness before its disposable-data cleanup.
    original = ROOT / "scripts/test-directory-artist-listings.sh"
    injected = original.with_name("studio-recovery-fixture.sh")
    needle = '  node "$repo_root/scripts/__tests__/studio-feed-http.mjs" "$api"'
    text = original.read_text()
    assert text.count(needle) == 1
    injected.write_text(text.replace(needle, needle + '\n  python3 "$repo_root/rehearsal/recovery.py" hook "$work_dir" "$server_pid"'))
    run("bash", injected, cwd=ROOT, env=dict(os.environ, TDF_ARTIST_LISTINGS_SERVER_BIN=str(CANDIDATE),
                                             TDF_ARTIST_LISTINGS_TEST_DATABASE_URL=db_url(19327)))


if __name__ == "__main__":
    if sys.argv[1:2] == ["hook"]:
        hook(Path(sys.argv[2]), int(sys.argv[3]))
    else:
        main()
