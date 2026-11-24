#!/usr/bin/perl
# MEDIA-MUSIC-LOUDNESS-001 (formal/system/music-loudness-check.md).
#
# Thin, total wrapper around FFmpeg's ebur128 filter. It measures one file and
# prints exactly one JSON object on standard output:
#
#   {"tool":"ffmpeg-ebur128-v1","state":"measured",
#    "integratedLufsTenths":-142,"truePeakDbtpTenths":-13}
#   {"tool":"ffmpeg-ebur128-v1","state":"failed","reason":"timeout"}
#
# Every outcome for a named file exits 0: a measurement that cannot be taken is
# a recorded failure, never a failure of the caller. Only a missing argument or
# an invalid limit exits 2. No transform is implemented here; values are the
# one-decimal figures FFmpeg prints, carried as integers in tenths.
use strict;
use warnings;
use POSIX qw(:sys_wait_h :errno_h);
use Time::HiRes qw(clock_gettime CLOCK_MONOTONIC);

my $TOOL = 'ffmpeg-ebur128-v1';
my $TAIL_BYTES = 65536;
my $HEAD_BYTES = 4096;

sub usage {
  print STDERR "Usage: $0 MASTER_FILE\n";
  exit 2;
}

sub bounded_integer {
  my ($name, $default, $minimum, $maximum) = @_;
  my $raw = defined $ENV{$name} && $ENV{$name} ne '' ? $ENV{$name} : $default;
  if ($raw !~ /\A[1-9][0-9]{0,5}\z/ || $raw < $minimum || $raw > $maximum) {
    print STDERR "$name must be an integer between $minimum and $maximum\n";
    exit 2;
  }
  return $raw + 0;
}

sub emit {
  my (%fields) = @_;
  my @parts = (qq{"tool":"$TOOL"}, qq{"state":"$fields{state}"});
  if ($fields{state} eq 'measured') {
    push @parts, qq{"integratedLufsTenths":$fields{integrated}};
    push @parts, qq{"truePeakDbtpTenths":$fields{peak}};
  } else {
    push @parts, qq{"reason":"$fields{reason}"};
  }
  push @parts, qq{"ffmpegVersion":"$fields{version}"} if defined $fields{version};
  local $| = 1;
  print '{' . join(',', @parts) . "}\n";
  exit 0;
}

# "-14.2" -> -142 without passing through floating point.
sub tenths {
  my ($text) = @_;
  return undef unless defined $text && $text =~ /\A(-?)([0-9]{1,3})\.([0-9])\z/;
  my $value = $2 * 10 + $3;
  return $1 eq '-' ? -$value : $value;
}

usage() unless @ARGV == 1 && length $ARGV[0];
my $source = $ARGV[0];
my $limit = bounded_integer('MUSIC_LOUDNESS_TIMEOUT_SECONDS', 900, 1, 3600);
my $grace = bounded_integer('MUSIC_LOUDNESS_KILL_GRACE_SECONDS', 5, 1, 60);
my $ffmpeg = defined $ENV{MUSIC_LOUDNESS_FFMPEG} && $ENV{MUSIC_LOUDNESS_FFMPEG} ne ''
  ? $ENV{MUSIC_LOUDNESS_FFMPEG} : 'ffmpeg';

emit(state => 'failed', reason => 'decode_error') unless -f $source && -r _;

pipe(my $reader, my $writer) or emit(state => 'failed', reason => 'tool_unavailable');
my $child = fork();
emit(state => 'failed', reason => 'tool_unavailable') unless defined $child;
if ($child == 0) {
  close $reader;
  open STDIN, '<', '/dev/null';
  open STDOUT, '>', '/dev/null';
  open STDERR, '>&', $writer;
  close $writer;
  # A leading "file:" keeps a name from being read as another protocol.
  no warnings 'exec';
  exec { $ffmpeg } $ffmpeg, '-nostdin', '-nostats', '-xerror',
    '-protocol_whitelist', 'file', '-i', "file:$source",
    '-map', '0:a:0', '-vn', '-sn', '-dn',
    '-af', 'ebur128=peak=true:framelog=verbose', '-f', 'null', '-';
  POSIX::_exit(127);
}
close $writer;

my $deadline = clock_gettime(CLOCK_MONOTONIC) + $limit;
my $kill_deadline = $deadline + $grace;
my ($timed_out, $killed, $cancelled) = (0, 0, 0);
for my $signal (qw(TERM INT HUP)) {
  $SIG{$signal} = sub { $cancelled = 1 };
}

my ($head, $tail) = ('', '');
my ($status, $reaped, $drained) = (0, 0, 0);
my $watched = '';
vec($watched, fileno($reader), 1) = 1;
while (1) {
  if (!$reaped && waitpid($child, WNOHANG) == $child) {
    $status = $?;
    $reaped = 1;
  }
  if ($cancelled) {
    unless ($reaped) {
      kill 'KILL', $child;
      waitpid $child, 0;
    }
    exit 143;
  }

  # A descendant may still hold stderr after FFmpeg exits. Apply the same
  # deadline to draining that pipe, and only signal an unreaped child.
  my $now = clock_gettime(CLOCK_MONOTONIC);
  if (!$timed_out && $now >= $deadline) {
    $timed_out = 1;
    kill 'TERM', $child unless $reaped;
  }
  last if $reaped && ($timed_out || $drained);
  if ($timed_out && !$killed && $now >= $kill_deadline) {
    kill 'KILL', $child;
    $killed = 1;
  }
  my $ready = 0;
  if ($drained) {
    select undef, undef, undef, 0.05;
  } else {
    $ready = select(my $readable = $watched, undef, undef, 0.2);
    if (defined $ready && $ready > 0) {
      my $count = sysread $reader, my $chunk, 16384;
      if (defined $count && $count > 0) {
        $head .= substr($chunk, 0, $HEAD_BYTES - length $head) if length $head < $HEAD_BYTES;
        $tail .= $chunk;
        $tail = substr($tail, -$TAIL_BYTES) if length $tail > $TAIL_BYTES;
      } elsif (defined $count || $! != EINTR) {
        $drained = 1;
      }
    }
  }
  $drained = 1 if $reaped && defined $ready && $ready == 0;
}
close $reader;

my $version;
$version = $1 if $head =~ /\Affmpeg version ([A-Za-z0-9.+~:_-]{1,64}) /;
my %common = (version => $version);

emit(%common, state => 'failed', reason => 'timeout') if $timed_out;
emit(%common, state => 'failed', reason => 'tool_unavailable')
  if WIFEXITED($status) && WEXITSTATUS($status) == 127;
emit(%common, state => 'failed', reason => 'decode_error') if $status != 0;

my $position = rindex $tail, 'Summary:';
emit(%common, state => 'failed', reason => 'no_summary') if $position < 0;
my $summary = substr $tail, $position;

my ($integrated_text) = $summary =~ /Integrated loudness:\s*\n\s+I:\s+(\S+) LUFS/;
my ($peak_text) = $summary =~ /True peak:\s*\n\s+Peak:\s+(\S+) dBFS/;
emit(%common, state => 'failed', reason => 'unparseable')
  unless defined $integrated_text && defined $peak_text;
# FFmpeg reports -70.0 LUFS when nothing passed the absolute gate and -inf for
# a channel that never left zero.
emit(%common, state => 'failed', reason => 'silent') if $peak_text eq '-inf';
my $integrated = tenths($integrated_text);
my $peak = tenths($peak_text);
emit(%common, state => 'failed', reason => 'unparseable')
  unless defined $integrated && defined $peak;
emit(%common, state => 'failed', reason => 'silent') if $integrated <= -700;
emit(%common, state => 'failed', reason => 'out_of_range')
  if $integrated > 100 || $peak < -1500 || $peak > 200;
emit(%common, state => 'measured', integrated => $integrated, peak => $peak);
