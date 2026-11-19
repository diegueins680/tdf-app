"""Checks the repository's actual receipt migration on a fresh PostgreSQL cluster."""
from pathlib import Path
import argparse
from concurrent.futures import ThreadPoolExecutor
import secrets
import socket
import subprocess
import tempfile
parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument("--pg-bin", type=Path, required=True)
parser.add_argument("--pg-share", type=Path, required=True)
args = parser.parse_args()
REPO = Path(__file__).resolve().parent.parent
RUNTIME = args.pg_bin.resolve()
SHARE = args.pg_share.resolve()
CANDIDATE=REPO/'tdf-hq/sql/2026-11-19_event_demo_request_namespace.sql'
ROLLBACK=REPO/'tdf-hq/sql/2026-11-19_event_demo_request_namespace_rollback.sql'
MIGRATION=REPO/'tdf-hq/sql/2026-09-18_ads_identity_requests.sql'
with tempfile.TemporaryDirectory(prefix='tdf-demo-receipt-test-') as d:
    p=Path(d); pw=p/'pwfile'; password=secrets.token_hex(24); pw.write_text(password); pw.chmod(0o600)
    with socket.socket() as port_socket:
        port_socket.bind(('127.0.0.1',0)); port=port_socket.getsockname()[1]
    env=dict(PATH=f"{RUNTIME}:/usr/bin:/bin", HOME=d, PGPASSWORD=password,
             PGHOST='127.0.0.1', PGPORT=str(port), PGUSER='synthetic', LANG='C.UTF-8')
    def run(*args,check=True,input=None):
        return subprocess.run(list(map(str,args)),env=env,check=check,capture_output=True,text=True,input=input)
    run(RUNTIME/'initdb','-D',p/'db','-L',SHARE,'-U','synthetic','--pwfile',pw,'--auth-local=reject','--auth-host=scram-sha-256')
    with (p/'db/postgresql.conf').open('a') as f:f.write(f"\nlisten_addresses='127.0.0.1'\nport={port}\nunix_socket_directories=''\n")
    run(RUNTIME/'pg_ctl','-D',p/'db','-l',p/'server.log','-w','start')
    try:
        run(RUNTIME/'createdb','tdf_demo_receipt_test')
        def sql(body,check=True):
            return run(RUNTIME/'psql','-X','-d','tdf_demo_receipt_test','-v','ON_ERROR_STOP=1','-v','VERBOSITY=verbose','-Atq',check=check,input=body)
        # Minimal tables and inert trigger dependency, not a full TDF schema or handler test.
        sql("""CREATE TABLE party(id bigint primary key);
        CREATE TABLE lead_interest(id bigint primary key);
        CREATE FUNCTION identity_reject_archived_reference() RETURNS trigger LANGUAGE plpgsql AS 'BEGIN RETURN NEW; END;';
        INSERT INTO party VALUES (1);
        INSERT INTO lead_interest SELECT generate_series(1,20);""")
        sql(MIGRATION.read_text())
        insert="INSERT INTO identity_ads_request (request_key,party_id,lead_interest_id,request_payload,response_payload,notification_state) VALUES"
        source_key='ae210a9d-aec3-4d0a-9118-cadba6335237'
        sql(f"{insert} ('{source_key}',1,1,'{{}}','{{}}','completed');")
        result=sql(f"{insert} ('demo:{source_key}',1,2,'{{}}','{{}}','completed');",check=False)
        assert result.returncode!=0 and '23514' in result.stderr and 'identity_ads_request_request_key_check' in result.stderr, result.stderr
        assert sql('SELECT count(*) FROM identity_ads_request;').stdout.strip()=='1'
        print('PostgreSQL 17 + unmodified receipt migration: legacy UUID accepted; demo:UUID rejected (23514 request_key check); 1 receipt total.')
        sql(CANDIDATE.read_text())
        sql(CANDIDATE.read_text())  # script itself can be safely reapplied
        def attempt(key, lead):
            return sql(f"{insert} ('{key}',1,{lead},'{{}}','{{}}','completed');",check=False)
        assert attempt('demo:'+source_key,2).returncode == 0
        assert attempt('a'*16,3).returncode == 0
        assert attempt('a'*128,4).returncode == 0
        assert attempt('demo:'+'a'*16,5).returncode == 0
        assert attempt('demo:'+'a'*128,6).returncode == 0
        for i,key in enumerate(('a'*15,'a'*129,'demo:'+'a'*15,'demo:'+'a'*129,'Demo:'+source_key,
                                'other:'+source_key,'demo:demo:'+source_key),7):
            result=attempt(key,i)
            assert result.returncode!=0 and '23514' in result.stderr, result.stderr
        race_key='demo:synthetic-concurrent-key-1'
        def one(index):
            cmd=f"""BEGIN;
            SELECT pg_advisory_xact_lock(hashtextextended('public-ads:' || '{race_key}', 0));
            INSERT INTO identity_ads_request (request_key,party_id,lead_interest_id,request_payload,response_payload,notification_state)
              SELECT '{race_key}',1,{index},'{{}}','{{}}','completed'
              WHERE NOT EXISTS(SELECT 1 FROM identity_ads_request WHERE request_key='{race_key}');
            COMMIT;"""
            sql(cmd)
        with ThreadPoolExecutor(max_workers=4) as pool:
            list(pool.map(one,range(15,19)))
        assert sql(f"SELECT count(*) FROM identity_ads_request where request_key='{race_key}';").stdout.strip()=='1'
        result=sql(ROLLBACK.read_text(),check=False)
        assert result.returncode!=0 and '23514' in result.stderr, result.stderr
        assert sql("SELECT count(*) FROM identity_ads_request WHERE request_key LIKE 'demo:%'").stdout.strip()=='4'
        print('Candidate migration: shared originating UUID and both length edges accepted separately for legacy Ads and Demo; seven out-of-contract inputs rejected. Four concurrent SQL transactions with the handler lock key yielded one receipt. Rollback refused without losing demo receipts.')
    finally:
        run(RUNTIME/'pg_ctl','-D',p/'db','-m','fast','-w','stop',check=False)
