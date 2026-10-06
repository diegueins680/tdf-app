"""Apply the user-authorized domain correction to the approved campaign video."""
from pathlib import Path
import subprocess
here = Path(__file__).resolve().parent
filters = (
    "scale=in_range=full:out_range=tv,format=yuv420p,"
    "drawbox=x=0:y=1470:w=1080:h=110:color=black:t=fill:enable='gte(t,12)',"
    "drawtext=fontfile='/System/Library/Fonts/Supplemental/Arial Bold.ttf':"
    "text='tdfrecords.net':fontcolor=0xCCADFF:fontsize=40:x=(w-tw)/2:y=1497:enable='gte(t,12)'"
)
subprocess.run([
    '/usr/local/bin/ffmpeg', '-hide_banner', '-loglevel', 'error', '-y',
    '-i', str(here / 'preview.mp4'), '-vf', filters,
    '-c:v', 'libx264', '-profile:v', 'high', '-level:v', '4.1',
    '-preset', 'veryfast', '-crf', '20', '-pix_fmt', 'yuv420p', '-color_range', 'tv',
    '-c:a', 'copy', '-movflags', '+faststart', str(here / 'preview-tdfrecords-net.mp4')
], check=True)
