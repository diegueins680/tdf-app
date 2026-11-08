#!/usr/bin/env python3
"""Validation controls for native Records delivery manifests."""
import copy, importlib.util, pathlib, unittest
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

if __name__=='__main__':unittest.main()
