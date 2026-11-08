#!/usr/bin/env python3
"""Validation controls for native Records delivery manifests."""
import copy, importlib.util, json, os, pathlib, re, stat, subprocess, sys, tempfile, unittest
from unittest import mock
p=pathlib.Path(__file__).with_name('prepare-native-session.py')
spec=importlib.util.spec_from_file_location('native',p);native=importlib.util.module_from_spec(spec);spec.loader.exec_module(native)

def manifest():
    return {'schemaVersion':1,'entity':{'kind':'recording','id':'11111111-1111-4111-8111-111111111111','expectedCode':'synthetic-source'},'title':'Synthetic session',
        'native':{'schemaVersion':1,'kind':'session','artist':'Synthetic','artistPartyIds':[1],'durationMs':30000,'state':'ready',
        'sources':[{'url':'https://example.invalid/video.mp4','quality':'high','mediaType':'video/mp4'},
                   {'url':'https://example.invalid/audio.m4a','quality':'high','mediaType':'audio/mp4'}],
        'chapters':[{'id':'22222222-2222-4222-8222-222222222222','title':"Song's title",'startMs':10000,'endMs':20000}],'credits':[]}}

class NativeSessionTests(unittest.TestCase):
    def test_ready_requires_independent_audio_and_video(self):
        m=manifest();native.validate(m)
        m['native']['sources'].pop()
        with self.assertRaisesRegex(ValueError,'independent audio'):native.validate(m)
    def test_provider_embed_cannot_become_native_source(self):
        for url in ('https://www.youtube.com/watch?v=synthetic','https://i.ytimg.com/vi/synthetic/default.jpg','http://example.invalid/video.mp4'):
            m=manifest();m['native']['sources'][0]['url']=url
            with self.assertRaises(ValueError):native.validate(m)
    def test_offsets_are_bounded_and_nonoverlapping(self):
        m=manifest();m['native']['chapters'][0]['endMs']=31000
        with self.assertRaises(ValueError):native.validate(m)
        m=manifest();chapter=copy.deepcopy(m['native']['chapters'][0]);chapter['id']='33333333-3333-4333-8333-333333333333';chapter['startMs']=15000
        m['native']['chapters'].append(chapter)
        with self.assertRaises(ValueError):native.validate(m)
    def test_identity_and_source_code_must_be_explicit(self):
        m=manifest();m['entity']['expectedCode']=''
        with self.assertRaises(ValueError):native.validate(m)
        m=manifest();m['native']['artistPartyIds']=[True]
        with self.assertRaises(ValueError):native.validate(m)
    def test_unknown_processing_state_is_rejected(self):
        m=manifest();m['native']['state']='published-somehow'
        with self.assertRaises(ValueError):native.validate(m)
    def test_missing_master_is_explicit_and_does_not_fabricate_sources(self):
        m=manifest();m['native']['state']='awaiting_master';m['native']['sources']=[]
        self.assertEqual(native.validate(m)['sources'],[])
    def test_chapter_uuid_cannot_alias_parent_or_repeat_with_another_spelling(self):
        m=manifest();m['native']['chapters'][0]['id']=m['entity']['id']
        with self.assertRaisesRegex(ValueError,'chapter identity'):native.validate(m)
        m=manifest();m['native']['chapters'][0]['id']='aaaaaaaa-1111-4111-8111-111111111111'
        second=copy.deepcopy(m['native']['chapters'][0]);second.update(id='AAAAAAAA-1111-4111-8111-111111111111',startMs=20000,endMs=25000)
        m['native']['chapters'].append(second)
        with self.assertRaisesRegex(ValueError,'chapter identity'):native.validate(m)
    def test_rendered_audit_does_not_invent_owner_or_master_evidence(self):
        m=manifest();m['native']['state']='awaiting_master';m['native']['sources']=[]
        m['localFiles']=[]
        sql=native.render_sql(m)
        self.assertNotIn('Owner requested',sql)
        self.assertNotIn('supplied the original master',sql)
        self.assertIn('"declaredState": "awaiting_master"',sql)
    def test_parent_audit_uses_returned_resource_identity_even_after_chapter_upserts(self):
        sql=native.render_sql(manifest())
        returned=re.findall(r'RETURNING id INTO ([a-z_]+)',sql)
        self.assertGreaterEqual(len(returned),2)
        audit=sql.split('INSERT INTO audit_log',1)[1]
        resource=re.search(r"'resourceId',\s*([a-z_]+)\s*,",audit)
        self.assertIsNotNone(resource,'Audit resourceId must use the parent upsert result, not a computed UUID')
        self.assertEqual(resource.group(1),returned[0])
        self.assertNotIn(returned[0],returned[1:],'Chapter upserts must not overwrite the captured parent identity')
    def test_cli_refuses_input_path_symlink_and_hardlink_without_modifying_manifest(self):
        for alias in ('same','symlink','hardlink'):
            with self.subTest(alias=alias), tempfile.TemporaryDirectory() as directory:
                source=pathlib.Path(directory)/'manifest.json';source.write_text(json.dumps(manifest()))
                original=source.read_bytes();output=source
                if alias!='same':
                    output=pathlib.Path(directory)/'output.sql'
                    if alias=='symlink':output.symlink_to(source)
                    else:os.link(source,output)
                result=subprocess.run([sys.executable,str(p),str(source),'--sql',str(output)],capture_output=True,text=True)
                self.assertNotEqual(result.returncode,0)
                self.assertEqual(source.read_bytes(),original)
                if alias=='symlink':self.assertTrue(output.is_symlink())
    def test_cli_regenerates_sql_with_private_atomic_replacement(self):
        with tempfile.TemporaryDirectory() as directory:
            source=pathlib.Path(directory)/'manifest.json';source.write_text(json.dumps(manifest()))
            output=pathlib.Path(directory)/'output.sql';output.write_text('old complete SQL');output.chmod(0o644)
            original_inode=output.stat().st_ino
            result=subprocess.run([sys.executable,str(p),str(source),'--sql',str(output)],capture_output=True,text=True)
            self.assertEqual(result.returncode,0,result.stderr)
            self.assertNotEqual(output.stat().st_ino,original_inode)
            self.assertEqual(stat.S_IMODE(output.stat().st_mode),0o600)
            self.assertEqual(json.loads(result.stdout)['sha256'],native.checksum(output))
            self.assertEqual(set(pathlib.Path(directory).iterdir()),{source,output})
    def test_atomic_replacement_failure_retains_previous_complete_sql_and_cleans_temporary(self):
        with tempfile.TemporaryDirectory() as directory:
            source=pathlib.Path(directory)/'manifest.json';source.write_text(json.dumps(manifest()))
            output=pathlib.Path(directory)/'output.sql';output.write_text('old complete SQL')
            def fail_replace(temporary,target):
                self.assertEqual(output.read_text(),'old complete SQL')
                self.assertEqual(stat.S_IMODE(pathlib.Path(temporary).stat().st_mode),0o600)
                self.assertIn('records_native_session_imported',pathlib.Path(temporary).read_text())
                raise OSError('synthetic atomic-replace failure')
            with mock.patch('os.replace',side_effect=fail_replace), mock.patch.object(sys,'argv',[str(p),str(source),'--sql',str(output)]):
                with self.assertRaisesRegex(OSError,'synthetic atomic-replace'):native.main()
            self.assertEqual(output.read_text(),'old complete SQL')
            self.assertEqual(set(pathlib.Path(directory).iterdir()),{source,output})

if __name__=='__main__':unittest.main()
