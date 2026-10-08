#!/usr/bin/env perl
# Worker-only path-style S3 uploader. Core Perl + curl + xmllint; no cloud SDK.
use strict;
use warnings;
use Digest::SHA qw(sha256_hex);
use File::Temp qw(tempdir);
use JSON::PP qw(encode_json);
use POSIX qw(WNOHANG);
use Time::HiRes qw(sleep);

umask 0077;
@ARGV == 4 or die "Usage: music-s3-upload.pl FILE BUCKET KEY MEDIA_TYPE\n";
my ($source, $bucket, $key, $media) = @ARGV;
for my $name (qw(MUSIC_S3_ENDPOINT MUSIC_S3_REGION MUSIC_S3_ACCESS_KEY_ID MUSIC_S3_SECRET_ACCESS_KEY)) {
  defined($ENV{$name}) && length($ENV{$name}) or die "$name is required\n";
}
my $endpoint = $ENV{MUSIC_S3_ENDPOINT};
$endpoint =~ s{/$}{};
$endpoint =~ m{\Ahttps://(?:[A-Za-z0-9.-]+|\[[A-Fa-f0-9:]+\])(?::[0-9]+)?\z}
  or die "S3 endpoint must be an HTTPS origin without credentials, path or query\n";
$bucket =~ /\A[a-z0-9][a-z0-9.-]{1,61}[a-z0-9]\z/ or die "Invalid S3 bucket\n";
length($key) && length($key) <= 1024 && $key !~ /[\x00-\x1f\x7f]/
  && !grep { $_ eq '' || $_ eq '.' || $_ eq '..' } split(m{/}, $key, -1)
  or die "Invalid S3 object key\n";
$media =~ m{\A[a-zA-Z0-9.+-]+/[a-zA-Z0-9.+-]+\z} or die "Invalid media type\n";
sub number {
  my ($name, $default, $min, $max) = @_;
  my $value = $ENV{$name} // $default;
  $value =~ /\A[1-9][0-9]{0,9}\z/ && $value >= $min && $value <= $max
    or die "$name is outside its supported range\n";
  return 0 + $value;
}
my $threshold = number('MUSIC_WORKER_MULTIPART_THRESHOLD_BYTES', 67108864, 5242880, 268435456);
my $part_size = number('MUSIC_WORKER_MULTIPART_PART_BYTES', 67108864, 5242880, 268435456);
my $timeout = number('MUSIC_WORKER_TRANSFER_TIMEOUT_SECONDS', 3600, 1, 86400);
open my $input, '<:raw', $source or die "Cannot open upload source\n";
-f $input or die "Upload source must be a regular file\n";
my $size = -s $input;
my $multipart = $size >= $threshold;
my $parts = $multipart ? int(($size + $part_size - 1) / $part_size) : 1;
$parts <= 10000 or die "Object requires more than 10000 parts; increase part size\n";
my $dir = tempdir('tdf-music-upload-XXXXXX', TMPDIR => 1, CLEANUP => 1);
my ($child, $upload_id, $completed, $interrupted);
sub stop_child {
  return unless $child;
  kill 'TERM', $child;
  for (1..50) {
    if (waitpid($child, WNOHANG) != 0) { undef $child; return; }
    sleep 0.1;
  }
  kill 'KILL', $child;
  waitpid($child, 0);
  undef $child;
}
for my $signal (qw(TERM INT)) {
  $SIG{$signal} = sub { $interrupted = 1; die "Storage transfer cancelled\n"; };
}
sub write_file {
  my ($file, $body) = @_;
  open my $out, '>:raw', $file or die "Cannot create upload temporary file\n";
  print {$out} $body or die "Cannot write upload temporary file\n";
  close $out or die "Cannot close upload temporary file\n";
}
sub read_file {
  my ($file) = @_;
  open my $in, '<:raw', $file or die "Cannot read storage response\n";
  local $/;
  return <$in> // '';
}
sub config_quote {
  my ($value) = @_;
  $value !~ /[\x00-\x1f\x7f]/ or die "Invalid storage credential/configuration characters\n";
  $value =~ s/(["\\])/\\$1/g;
  return qq{"$value"};
}
# Secrets travel over stdin, never in arguments, temporary files or reports.
# -q disables .curlrc (including accidental redirects or insecure TLS settings).
my $config = 'user = ' . config_quote("$ENV{MUSIC_S3_ACCESS_KEY_ID}:$ENV{MUSIC_S3_SECRET_ACCESS_KEY}") . "\n";
$config .= 'aws-sigv4 = ' . config_quote("aws:amz:$ENV{MUSIC_S3_REGION}:s3") . "\n";
if (length($ENV{MUSIC_S3_SESSION_TOKEN} // '')) {
  $config .= 'header = ' . config_quote("x-amz-security-token: $ENV{MUSIC_S3_SESSION_TOKEN}") . "\n";
}
sub execute {
  my (@args) = @_;
  pipe(my $reader, my $writer) or die "Cannot create storage configuration pipe\n";
  $child = fork();
  defined $child or die "Cannot start storage command\n";
  if ($child == 0) {
    $SIG{TERM} = $SIG{INT} = 'DEFAULT';
    close $writer;
    open STDIN, '<&', $reader or POSIX::_exit(127);
    close $reader;
    open STDOUT, '>:raw', "$dir/stdout" or POSIX::_exit(127);
    open STDERR, '>:raw', "$dir/stderr" or POSIX::_exit(127);
    exec { $args[0] } @args or POSIX::_exit(127);
  }
  close $reader;
  {
    local $SIG{PIPE} = 'IGNORE';
    print {$writer} ($args[0] eq 'curl' ? $config : '') or die "Cannot pass storage configuration\n";
    close $writer or die "Cannot close storage configuration pipe\n";
  }
  my $pid = waitpid($child, 0);
  my $status = $?;
  undef $child;
  $pid > 0 && $status == 0 or die "Storage command failed (status $status); retry job or check provider permissions/network\n";
}
sub uri {
  my ($value) = @_;
  $value =~ s/([^A-Za-z0-9_.~-])/sprintf('%%%02X', ord($1))/ge;
  return $value;
}
my $url = "$endpoint/$bucket/" . join('/', map { uri($_) } split m{/}, $key);
sub request {
  my ($method, $query, $retries, $limit, @args) = @_;
  execute('curl', '-q', '--config', '-', '--fail', '--silent', '--show-error',
    '--proto', '=https', '--connect-timeout', '10', '--max-time', $limit,
    '--retry', $retries, '--retry-max-time', $limit, '--retry-all-errors',
    '--max-filesize', '1048576', '-D', "$dir/headers", '-o', "$dir/response",
    '-X', $method, @args, "$url$query");
}
sub scalar_xml {
  my ($root, $field) = @_;
  my $body = read_file("$dir/response");
  $body !~ /<!|\x00/ && length($body) <= 1048576 or die "Unsafe storage XML\n";
  my $path = "/*[local-name()='$root']/*[local-name()='$field']";
  execute('xmllint', '--nonet', '--xpath', "count($path)", "$dir/response");
  read_file("$dir/stdout") eq '1' || read_file("$dir/stdout") eq "1\n"
    or die "Storage response missing unique $root/$field (including HTTP 200 errors)\n";
  execute('xmllint', '--nonet', '--xpath', "string($path)", "$dir/response");
  my $value = read_file("$dir/stdout");
  $value =~ s/\n\z//; # xmllint versions differ on their trailing newline.
  length($value) && $value !~ /[\x00-\x1f\x7f]/ or die "Invalid storage XML scalar\n";
  return $value;
}
sub xml_escape {
  my ($value) = @_;
  $value =~ s/&/&amp;/g; $value =~ s/</&lt;/g; $value =~ s/>/&gt;/g;
  $value =~ s/"/&quot;/g; $value =~ s/'/&apos;/g;
  return $value;
}
my $ok = eval {
  my $initial_hash = Digest::SHA->new(256)->addfile($input)->hexdigest;
  seek($input, 0, 0) or die "Cannot rewind source\n";
  if (!$multipart) {
    request('PUT', '', 3, $timeout, '-H', "Content-Type: $media",
      '-H', "x-amz-content-sha256: $initial_hash", '--upload-file', $source);
    Digest::SHA->new(256)->addfile($input)->hexdigest eq $initial_hash
      or die "Upload source changed during transfer\n";
  } else {
    # Create/complete are deliberately not retried blindly: a lost response is
    # ambiguous. A job retry uses a fresh upload at the same immutable object key.
    request('POST', '?uploads=', 0, $timeout, '-H', "Content-Type: $media");
    $upload_id = scalar_xml('InitiateMultipartUploadResult', 'UploadId');
    my $query = '?uploadId=' . uri($upload_id);
    my $whole = Digest::SHA->new(256);
    my $completion = '<CompleteMultipartUpload>';
    my $sent = 0;
    for my $part (1..$parts) {
      open my $out, '>:raw', "$dir/part" or die "Cannot create bounded part file\n";
      my $hash = Digest::SHA->new(256);
      my $remaining = $size - $sent < $part_size ? $size - $sent : $part_size;
      while ($remaining > 0) {
        my $read = read($input, my $buffer, $remaining < 1048576 ? $remaining : 1048576);
        defined($read) && $read > 0 or die "Upload source truncated or unreadable\n";
        print {$out} $buffer or die "Cannot write bounded part file\n";
        $whole->add($buffer); $hash->add($buffer); $remaining -= $read; $sent += $read;
      }
      close $out or die "Cannot close bounded part file\n";
      request('PUT', "?partNumber=$part&uploadId=" . uri($upload_id), 3, $timeout,
        '-H', 'x-amz-content-sha256: ' . $hash->hexdigest, '--upload-file', "$dir/part");
      # Ignore interim/retried response headers; accept exactly one final ETag.
      my $headers = read_file("$dir/headers");
      my @blocks = split /(?=HTTP\/\S+ \d{3})/, $headers;
      my @etags = ($blocks[-1] // '') =~ /^etag:[ \t]*("[^"\x00-\x1f\x7f]+")\r?$/gmi;
      @etags == 1 or die "UploadPart response missing unique quoted ETag\n";
      $completion .= '<Part><PartNumber>' . $part . '</PartNumber><ETag>' .
        xml_escape($etags[0]) . '</ETag></Part>';
      print STDERR "Music S3 multipart part $part/$parts accepted ($sent/$size bytes)\n";
    }
    my $extra = read($input, my $byte, 1);
    defined($extra) && $extra == 0 && $whole->hexdigest eq $initial_hash
      or die "Upload source changed; multipart completion blocked\n";
    write_file("$dir/complete.xml", "$completion</CompleteMultipartUpload>");
    request('POST', $query, 0, $timeout, '-H', 'Content-Type: application/xml',
      '-H', 'x-amz-content-sha256: ' . sha256_hex(read_file("$dir/complete.xml")),
      '--data-binary', "\@$dir/complete.xml");
    scalar_xml('CompleteMultipartUploadResult', 'ETag');
    $completed = 1;
  }
  print encode_json({ bytes => $size, sha256 => $initial_hash,
    mode => $multipart ? 'multipart' : 'single', parts => $parts }), "\n";
  1;
};
my $error = $@;
if (!$ok) {
  $SIG{TERM} = $SIG{INT} = 'IGNORE';
  stop_child();
  if (defined($upload_id) && !$completed) {
    my $aborted = eval { request('DELETE', '?uploadId=' . uri($upload_id), 0, 15); 1 };
    print STDERR $aborted ? "Music S3 incomplete upload aborted\n" :
      "Music S3 abort unconfirmed; reconcile incomplete uploads via bucket lifecycle\n";
  }
  print STDERR $error;
  exit($interrupted ? 143 : 1);
}
