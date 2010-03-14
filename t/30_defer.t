#!/usr/bin/perl
#
# Check ->defer and ->flush methods
#

use POSIX 'SEEK_SET';
my $file = "tf$$.txt";
$: = Tie::File::_default_recsep();
my $data = "rec0$:rec1$:rec2$:";
my ($o, $n);

print "1..92\n";

my $N = 1;
use Tie::File;
print "ok $N\n"; $N++;

open F, "> $file" or die $!;
binmode F;
print F $data;
close F;
$o = tie @a, 'Tie::File', $file, autodefer => 0;
print $o ? "ok $N\n" : "not ok $N\n";
$N++;

# (3-6) Deferred storage
$o->defer;
$a[3] = "rec3";
check_contents($data);          # nothing written yet
$a[4] = "rec4";
check_contents($data);          # nothing written yet

# (7-8) Flush
$o->flush;
check_contents($data . "rec3$:rec4$:");          # now it's written

# (9-12) Deferred writing disabled?
$a[3] = "rec9";
check_contents("${data}rec9$:rec4$:");
$a[4] = "rec8";
check_contents("${data}rec9$:rec8$:");

# (13-18) Now let's try two batches of records
$#a = 2;
$o->defer;
$a[0] = "record0";
check_contents($data);          # nothing written yet
$a[2] = "record2";
check_contents($data);          # nothing written yet
$o->flush;
check_contents("record0$:rec1$:record2$:");

# (19-22) Deferred writing past the end of the file
$o->defer;
$a[4] = "record4";
check_contents("record0$:rec1$:record2$:");
$o->flush;
check_contents("record0$:rec1$:record2$:$:record4$:");


# (23-26) Now two long batches
$o->defer;
for (0..2, 4..6) {
  $a[$_] = "r$_";
}
check_contents("record0$:rec1$:record2$:$:record4$:");
$o->flush;
check_contents(join $:, "r0".."r2", "", "r4".."r6", "");

# (27-30) Now let's make sure that discarded writes are really discarded
# We have a 2Mib buffer here, so we can be sure that we aren't accidentally
# filling it up
$o->defer;
for (0, 3, 7) {
  $a[$_] = "discarded$_";
}
check_contents(join $:, "r0".."r2", "", "r4".."r6", "");
$o->discard;
check_contents(join $:, "r0".."r2", "", "r4".."r6", "");

################################################################
#
# Now we're going to test the results of a small memory limit
#
# 
undef $o;  untie @a;
$data = join "$:", map("record$_", 0..7), "";  # records are 8 or 9 bytes long
open F, "> $file" or die $!;
binmode F;
print F $data;
close F;

# Limit cache+buffer size to 47 bytes 
my $MAX = 25;
#  -- that's enough space for 3 records, but not 4, on both \n and \r\n systems
$o = tie @a, 'Tie::File', $file, memory => $MAX, autodefer => 0;
print $o ? "ok $N\n" : "not ok $N\n";
$N++;

# (31-32) Fill up the read cache
my @z;
@z = @a;
# the cache now contains records 5,6,7.
check_caches({map(($_ => "record$_"), 5..7)},
             []);

# (33-47) See if overloading the defer starts by flushing the read cache
# and then flushes out the defer
$o->defer;
$a[0] = "recordA";              # That should flush record 5 from the cache
check_caches({0 => "recordA", map(($_ => "record$_"), 6..7)},
             [0]);
check_contents($data);

$a[1] = "recordB";              # That should flush record 6 from the cache
check_caches({0 => "recordA", 1 => "recordB", 7 => "record7"},
             [0..1]);
check_contents($data);

$a[2] = "recordC";              # That should flush record 7 from the cache
check_caches({0 => "recordA", 1 => "recordB", 2 => "recordC"},
             [0..2]);
check_contents($data);

$a[3] = "recordD";              # This should commit the writes and
                                # expire record 0 from the cache
check_caches({1 => "recordB", 2 => "recordC", 3 => "recordD"},
             []);
check_contents(join ($:, qw(recordA recordB recordC recordD
                            record4 record5 record6 record7), ""));


$a[0] = "recordE";        # even though we flushed, deferring is STILL ENABLED
check_caches({0 => "recordE", 2 => "recordC", 3 => "recordD"},
             [0]);
check_contents(join ($:, qw(recordA recordB recordC recordD
                            record4 record5 record6 record7), ""));

# Check readcache-deferbuffer interactions

# (48-50) This should remove outdated data from the read cache
$a[2] = "recordF";
check_caches({0 => "recordE", 2 => "recordF", 3 => "recordD"},
             [0, 2]);
check_contents(join ($:, qw(recordA recordB recordC recordD
                            record4 record5 record6 record7), ""));

# (51-54) This should read back out of the defer buffer
# without adding anything to the read cache
my $z;
$z = $a[2];
print $z eq "recordF" ? "ok $N\n" : "not ok $N\n";  $N++;
check_caches({0 => "recordE", 2 => "recordF", 3 => "recordD"},
             [0, 2]);
check_contents(join ($:, qw(recordA recordB recordC recordD
                            record4 record5 record6 record7), ""));

# (55-58) This should repopulate the read cache with a new record
$z = $a[1];
print $z eq "recordB" ? "ok $N\n" : "not ok $N\n";  $N++;
check_caches({0 => "recordE", 1 => "recordB", 2 => "recordF"},
             [0, 2]);
check_contents(join ($:, qw(recordA recordB recordC recordD
                            record4 record5 record6 record7), ""));

# (59-62) This should FLUSH the deferred buffer
# This incidentally reads record 3 into the cache,
# flushing out record 1.
$z = splice @a, 3, 1, "recordZ";
print $z eq "recordD" ? "ok $N\n" : "not ok $N\n";  $N++;
check_caches({0 => "recordE", 2 => "recordF", 3 => "recordZ"},
             []);
check_contents(join ($:, qw(recordE recordB recordF recordZ
                            record4 record5 record6 record7), ""));

# (63-65) We should STILL be in deferred writing mode
$a[5] = "recordX";
check_caches({2 => "recordF", 3 => "recordZ", 5 => "recordX"},
             [5]);
check_contents(join ($:, qw(recordE recordB recordF recordZ
                            record4 record5 record6 record7), ""));

# (66-68) This should OVERWRITE the existing deferred record
# and NOT flush the buffer
$a[5] = "recordQ";
check_caches({2 => "recordF", 3 => "recordZ", 5 => "recordQ"},
             [5]);
check_contents(join ($:, qw(recordE recordB recordF recordZ
                            record4 record5 record6 record7), ""));


# (69-71) Discard should just dump the whole deferbuffer
$o->discard;
check_caches({2 => "recordF", 3 => "recordZ"},
             []);
check_contents(join ($:, qw(recordE recordB recordF recordZ
                            record4 record5 record6 record7), ""));

# (72-74) NOW we are out of deferred writing mode
$a[0] = "recordG";
check_caches({0 => "recordG", 2 => "recordF", 3 => "recordZ"},
             []);
check_contents(join ($:, qw(recordG recordB recordF recordZ
                            record4 record5 record6 record7), ""));

# Now try some tricky cases.
$o->defer;
# (75-77) This loads up the cache with deferred records
$a[0] = "recordP";
$a[1] = "recordQ";
$a[3] = "recordS";
check_caches({0 => "recordP", 1 => "recordQ", 3 => "recordS"},
             [0, 1, 3]);
check_contents(join ($:, qw(recordG recordB recordF recordZ
                            record4 record5 record6 record7), ""));

# (78-81) Please do *not* cause an infinite loop!
$z = splice(@a, 2, 1, "recordR");
print $z eq "recordF" ? "ok $N\n" : "not ok $N\n";  $N++;
check_caches({1 => "recordQ", 2 => "recordR", 3 => "recordS"},
             []);
check_contents(join ($:, qw(recordP recordQ recordR recordS
                            record4 record5 record6 record7), ""));

# (82-84) Loads up the cache with deferred records again
$a[0] = "recordT";
$a[1] = "recordU";
$a[3] = "recordW";
check_caches({0 => "recordT", 1 => "recordU", 3 => "recordW"},
             [0, 1, 3]);
check_contents(join ($:, qw(recordP recordQ recordR recordS
                            record4 record5 record6 record7), ""));

# (85-88) Please do *not* cause an infinite loop!
$z = splice(@a, 4, 4, "record0", "record1", "record2", "record3");
print $z eq "record7" ? "ok $N\n" : "not ok $N\n";  $N++;
check_caches({5 => "record1", 6 => "record2", 7 => "record3"},
             []);
check_contents(join ($:, qw(recordT recordU recordR recordW
                            record0 record1 record2 record3), ""));


# (89-92) Last call--untying the array should flush the deferbuffer
$a[0] = "flushed";
check_caches({0 => "flushed", 6 => "record2", 7 => "record3"},
             [0]);
check_contents(join ($:, qw(recordT recordU recordR recordW
                            record0 record1 record2 record3), ""));
undef $o;
untie @a;
# (92) We can't use check_contents any more, because the object is dead
open F, "< $file" or die;
binmode F;
{ local $/ ; $z = <F> }
close F;
my $x = join($:, qw(flushed recordU recordR recordW
                    record0 record1 record2 record3), "");
if ($z eq $x) {
  print "ok $N\n";
} else {
  my $msg = ctrlfix("expected <$x>, got <$z>");
  print "not ok $N \# $msg\n";
}
$N++;

################################################################


sub check_caches {
  my ($xcache, $xdefer) = @_;

  $xcache = {map {$_ => "$xcache->{$_}$:"} keys %$xcache};
  $xdefer = {map {$_ => 1} @$xdefer};

  my $good = 1;
  $good &&= hash_equal($o->{cache}, $xcache, "true cache", "expected cache");
  $good &&= hash_equal($o->{deferred}, $xdefer, "true defer", "expected defer");
  print $good ? "ok $N\n" : "not ok $N\n";
  $N++;
}

sub hash_equal {
  my ($a, $b, $ha, $hb) = @_;
  $ha = 'first hash'  unless defined $ha;
  $hb = 'second hash' unless defined $hb;

  my $good = 1;
  my %b_seen;

  for my $k (keys %$a) {
    if (! exists $b->{$k}) {
      print ctrlfix("# Key $k is in $ha but not $hb"), "\n";
      $good = 0;
    } elsif ($b->{$k} ne $a->{$k}) {
      print ctrlfix("# Key $k is <$a->{$k}> in $ha but <$b->{$k}> in $hb"), "\n";
      $b_seen{$k} = 1;
      $good = 0;
    } else {
      $b_seen{$k} = 1;
    }
  }

  for my $k (keys %$b) {
    unless ($b_seen{$k}) {
      print ctrlfix("# Key $k is in $hb but not $ha"), "\n";
      $good = 0;
    }
  }

  $good;
}


sub check_contents {
  my $x = shift;
  

  my $integrity = $o->_check_integrity($file, $ENV{INTEGRITY});
  print $integrity ? "ok $N\n" : "not ok $N\n";
  $N++;

  local *FH = $o->{fh};
  seek FH, 0, SEEK_SET;

  my $a;
  { local $/; $a = <FH> }
  $a = "" unless defined $a;
  if ($a eq $x) {
    print "ok $N\n";
  } else {
    my $msg = ctrlfix("# expected <$x>, got <$a>");
    print "not ok $N\n$msg\n";
  }
  $N++;
}

sub ctrlfix {
  local $_ = shift;
  s/\n/\\n/g;
  s/\r/\\r/g;
  $_;
}

END {
  1 while unlink $file;
}

