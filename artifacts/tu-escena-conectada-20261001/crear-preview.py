"""Rebuild the campaign preview from existing TDF footage and an authentic capture.

The instrumental is newly composed procedural synthesis; no recordings,
third-party samples, external generation service or artist imitation are used.
"""
from pathlib import Path
import subprocess
import wave
import numpy as np

HERE = Path(__file__).resolve().parent
ROOT = HERE.parent.parent
SR, DURATION, BPM = 48000, 15, 128
beat = 60 / BPM
audio = np.zeros((SR * DURATION, 2), dtype=np.float64)
rng = np.random.default_rng(20261001)

def put(start, sound, gain=1.0, pan=0.0):
    index = round(start * SR)
    sound = sound[: max(0, len(audio) - index)] * gain
    audio[index:index+len(sound), 0] += sound * (1 - max(0, pan))
    audio[index:index+len(sound), 1] += sound * (1 + min(0, pan))

def note(midi):
    return 440 * 2 ** ((midi - 69) / 12)

# Eight bars, Am7 / Fmaj7 / Cmaj7 / G6, with a simple syncopated pluck.
chords = [(57,60,64,67), (53,57,60,64), (48,52,55,59), (55,59,62,64)] * 2
for bar, chord in enumerate(chords):
    t = np.arange(round(beat*4*SR)) / SR
    env = np.minimum(t/.15, 1) * np.minimum((t[-1]-t)/.22, 1)
    pad = sum(np.sin(2*np.pi*note(m)*t + .13*i) for i,m in enumerate(chord))/4
    put(bar*beat*4, pad*env, .09, (-1 if bar%2 else 1)*.2)
    for step, degree in [(0,0),(1.5,2),(2.5,1),(3.5,3)]:
        t = np.arange(round(.36*SR))/SR
        hz = note(chord[degree]+12)
        tone=(np.sin(2*np.pi*hz*t)+.25*np.sin(2*np.pi*hz*2*t))
        put((bar*4+step)*beat, tone*np.exp(-t*14)*np.minimum(t/.005,1), .11, .25 if degree%2 else -.25)
    for step in [0,1.5,2,3.5]:
        t=np.arange(round(.24*SR))/SR
        put((bar*4+step)*beat,np.sin(2*np.pi*note(chord[0]-12)*t)*np.exp(-t*9)*np.minimum(t/.007,1),.15)

for step in range(32):
    t=np.arange(round(.28*SR))/SR
    # Analytic phase for a decaying sine kick, with a short noise transient.
    phase=2*np.pi*(48*t + (110/28)*(1-np.exp(-28*t)))
    kick=np.sin(phase)*np.exp(-t*17)
    put(step*beat,kick,.35)
    if step%2:
        t=np.arange(round(.13*SR))/SR
        noise=rng.standard_normal(len(t))
        clap=np.diff(noise,prepend=0)*np.exp(-t*35)*np.minimum(t/.002,1)
        put(step*beat,clap,.065)
for step in range(64):
    t=np.arange(round(.07*SR))/SR
    noise=rng.standard_normal(len(t))
    hat=np.diff(noise,prepend=0)*np.exp(-t*80)
    put(step*beat/2,hat,.018 if step%2==0 else .026, .35 if step%2 else -.35)

t=np.arange(len(audio))/SR
fade=np.minimum(t/.04,1)*np.minimum((DURATION-t)/.55,1)
audio=np.tanh(audio*1.25)*fade[:,None]
audio*=.83/max(np.max(np.abs(audio)),1e-9)
with wave.open(str(HERE/'instrumental-original.wav'),'wb') as out:
    out.setnchannels(2)
    out.setsampwidth(2)
    out.setframerate(SR)
    out.writeframes((audio*32767).astype('<i2').tobytes())

font='/System/Library/Fonts/Supplemental/Arial Bold.ttf'
filter_graph=(
    '[0:v]split=2[start][end];'
    '[start]trim=start=0:end=6,setpts=PTS-STARTPTS,setsar=1[v0];'
    '[end]trim=start=10:end=15,setpts=PTS-STARTPTS,setsar=1[v2];'
    '[1:v]scale=1080:1920,setsar=1,fps=30,trim=duration=4,setpts=PTS-STARTPTS,'
    'drawbox=x=0:y=1190:w=1080:h=390:color=black@0.82:t=fill,'
    f"drawtext=fontfile='{font}':text='PERFILES. ARTISTAS.':fontcolor=white:fontsize=54:x=(w-tw)/2:y=1245,"
    f"drawtext=fontfile='{font}':text='LANZAMIENTOS. EXPERIENCIAS.':fontcolor=white:fontsize=43:x=(w-tw)/2:y=1340,"
    f"drawtext=fontfile='{font}':text='Tu escena, conectada.':fontcolor=0xCCADFF:fontsize=38:x=(w-tw)/2:y=1450[v1];"
    '[v0][v1][v2]concat=n=3:v=1:a=0,format=yuv420p[outv];'
    '[2:a]loudnorm=I=-16:TP=-1.5:LRA=9,atrim=duration=15,asetpts=PTS-STARTPTS[outa]'
)
subprocess.run([
    '/usr/local/bin/ffmpeg','-hide_banner','-loglevel','warning','-y',
    '-i',str(ROOT/'artifacts/tu-escena-conectada-piloto-preview-public.mp4'),
    '-loop','1','-framerate','30','-i',str(HERE/'plataforma-publica.png'),
    '-i',str(HERE/'instrumental-original.wav'),
    '-filter_complex',filter_graph,'-map','[outv]','-map','[outa]',
    '-t','15','-r','30','-fps_mode','cfr','-c:v','libx264','-profile:v','high','-level:v','4.1','-preset','veryfast','-crf','20',
    '-c:a','aac','-ar','48000','-b:a','192k','-movflags','+faststart',
    str(HERE/'preview.mp4')
],check=True)
