#!/usr/bin/perl
#
# Check behavior of 'autodefer' feature
# Mostly this isn't implemented yet
# This file is primarily here to make sure that the promised ->autodefer
# method doesn't croak.
#

use POSIX 'SEEK_SET';

my $file = "tf$$.txt";
$: = Tie::File::_default_recsep();
my $data = "rec0$:rec1$:rec2$:";
my ($o, $n, @a);

print "1..56\n";

my $N = 1;
use Tie::File;
print "ok $N\n"; $N++;

open F, "> $file" or die $!;
binmode F;
print F $data;
close F;
$o = tie @a, 'Tie::File', $file;
print $o ? "ok $N\n" : "not ok $N\n";
$N++;

# I am an undocumented feature
$o->{autodefer_filelen_threshhold} = 0;
# Normally autodeferring only works on large files.  This disables that.

# (3-22) Deferred storage
$a[3] = "rec3";
check_autodeferring('OFF');
$a[4] = "rec4";
check_autodeferring('OFF');
$a[5] = "rec5";
check_autodeferring('ON');
check_contents($data . "rec3$:rec4$:"); # only the first two were written
$a[6] = "rec6";
check_autodeferring('ON');
check_contents($data . "rec3$:rec4$:"); # still nothing written
$a[7] = "rec7";
check_autodeferring('ON');
check_contents($data . "rec3$:rec4$:"); # still nothing written
$a[0] = "recX";
check_autodeferring('OFF');
check_contents("recX$:rec1$:rec2$:rec3$:rec4$:rec5$:rec6$:rec7$:");
$a[1] = "recY";
check_autodeferring('OFF');
check_contents("recX$:recY$:rec2$:rec3$:rec4$:rec5$:rec6$:rec7$:");
$a[2] = "recZ";                 # it kicks in here
check_autodeferring('ON');
check_contents("recX$:recY$:rec2$:rec3$:rec4$:rec5$:rec6$:rec7$:");

# (23-26) Explicitly enabling deferred writing deactivates autodeferring
$o->defer;
check_autodeferring('OFF');
check_contents("recX$:recY$:recZ$:rec3$:rec4$:rec5$:rec6$:rec7$:");
$o->discard;
check_autodeferring('OFF');

# (27-32) Now let's try the CLEAR special case
@a = ("r0" .. "r4");
check_autodeferring('ON');
# The file was extended to the right length, but nothing was actually written.
check_contents("$:$:$:$:$:");
$a[2] = "fish";
check_autodeferring('OFF');
check_contents("r0$:r1$:fish$:r3$:r4$:");

# (33-47) Now let's try the originally intended application:  a 'for' loop.
my $it = 0;
for (@a) {
  $_ = "##$_";
  if ($it == 0) {
    check_autodeferring('OFF');
    check_contents("##r0$:r1$:fish$:r3$:r4$:");
  } elsif ($it == 1) {
    check_autodeferring('OFF');
    check_contents("##r0$:##r1$:fish$:r3$:r4$:");
  } else {
    check_autodeferring('ON');
    check_contents("##r0$:##r1$:fish$:r3$:r4$:");
  }
  $it++;
}

# (48-56) Autodeferring should not become active during explicit defer mode
$o->defer();  # This should flush the pending autodeferred records
              # and deactivate autodeferring
check_autodeferring('OFF');
check_contents("##r0$:##r1$:##fish$:##r3$:##r4$:");
@a = ("s0" .. "s4");
check_autodeferring('OFF');
check_contents("");
$o->flush;
check_autodeferring('OFF');
check_contents("s0$:s1$:s2$:s3$:s4$:");

exit 0;

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
my $MAX = 47;
#  -- that's enough space for 5 records, but not 6, on both \n and \r\n systems
my $BUF = 20;
#  -- that's enough space for 2 records, but not 3, on both \n and \r\n systems
$o = tie @a, 'Tie::File', $file, memory => $MAX, dw_size => $BUF;
print $o ? "ok $N\n" : "not ok $N\n";
$N++;

# (31-32) Fill up the read cache
my @z;
@z = @a;                        
# the cache now contains records 3,4,5,6,7.
check_caches({map(($_ => "record$_$:"), 3..7)}, 
             {});

# (33-44) See if overloading the defer starts by flushing the read cache
# and then flushes out the defer
$o->defer;
$a[0] = "recordA";              # That should flush record 3 from the cache
check_caches({map(($_ => "record$_$:"), 4..7)}, 
             {0 => "recordA$:"});
check_contents($data);

$a[1] = "recordB";              # That should flush record 4 from the cache
check_caches({map(($_ => "record$_$:"), 5..7)}, 
             {0 => "recordA$:",
              1 => "recordB$:"});
check_contents($data);

$a[2] = "recordC";              # That should flush the whole darn defer
# Flushing the defer requires looking up the true lengths of records
# 0..2, which flushes out the read cache, leaving only 1..2 there.
# Then the splicer updates the cached versions of 1..2 to contain the
# new data
check_caches({1 => "recordB$:", 2 => "recordC$:"},
             {});               # URRRP
check_contents(join("$:", qw(recordA recordB recordC 
                             record3 record4 record5 record6 record7)) . "$:");

$a[3] = "recordD";         # even though we flushed, deferring is STILL ENABLED
check_caches({1 => "recordB$:", 2 => "recordC$:"},
             {3 => "recordD$:"}); 
check_contents(join("$:", qw(recordA recordB recordC 
                             record3 record4 record5 record6 record7)) . "$:");

# Check readcache-deferbuffer interactions

# (45-47) This should remove outdated data from the read cache
$a[2] = "recordE";
check_caches({1 => "recordB$:",                 },
             {3 => "recordD$:", 2 => "recordE$:"}); 
check_contents(join("$:", qw(recordA recordB recordC 
                             record3 record4 record5 record6 record7)) . "$:");

# (48-51) This should read back out of the defer buffer 
# without adding anything to the read cache
my $z;
$z = $a[2];
print $z eq "recordE" ? "ok $N\n" : "not ok $N\n";  $N++;
check_caches({1 => "recordB$:",                 },
             {3 => "recordD$:", 2 => "recordE$:"}); 
check_contents(join("$:", qw(recordA recordB recordC 
                             record3 record4 record5 record6 record7)) . "$:");

# (52-55) This should repopulate the read cache with a new record
$z = $a[0];
print $z eq "recordA" ? "ok $N\n" : "not ok $N\n";  $N++;
check_caches({1 => "recordB$:", 0 => "recordA$:"},
             {3 => "recordD$:", 2 => "recordE$:"}); 
check_contents(join("$:", qw(recordA recordB recordC 
                             record3 record4 record5 record6 record7)) . "$:");

# (56-59) This should flush the LRU record from the read cache
$z = $a[4];  $z = $a[5];
print $z eq "record5" ? "ok $N\n" : "not ok $N\n";  $N++;
check_caches({5 => "record5$:", 0 => "recordA$:", 4 => "record4$:"},
             {3 => "recordD$:", 2 => "recordE$:"}); 
check_contents(join("$:", qw(recordA recordB recordC 
                             record3 record4 record5 record6 record7)) . "$:");

# (60-63) This should FLUSH the deferred buffer
# In doing so, it will read in records 2 and 3, flushing 0 and 4
# from the read cache, leaving 2, 3, and 5.
$z = splice @a, 3, 1, "recordZ";
print $z eq "recordD" ? "ok $N\n" : "not ok $N\n";  $N++;
check_caches({5 => "record5$:", 3 => "recordZ$:", 2 => "recordE$:"},
             {}); 
check_contents(join("$:", qw(recordA recordB recordE 
                             recordZ record4 record5 record6 record7)) . "$:");

# (64-66) We should STILL be in deferred writing mode
$a[5] = "recordX";
check_caches({3 => "recordZ$:", 2 => "recordE$:"},
             {5 => "recordX$:"}); 
check_contents(join("$:", qw(recordA recordB recordE 
                             recordZ record4 record5 record6 record7)) . "$:");

# Fill up the defer buffer again
$a[4] = "recordP";
# (67-69) This should OVERWRITE the existing deferred record 
# and NOT flush the buffer
$a[5] = "recordQ";   
check_caches({3 => "recordZ$:", 2 => "recordE$:"},
             {5 => "recordQ$:", 4 => "recordP$:"}); 
check_contents(join("$:", qw(recordA recordB recordE 
                             recordZ record4 record5 record6 record7)) . "$:");


# (70-72) Discard should just dump the whole deferbuffer
$o->discard;
check_caches({3 => "recordZ$:", 2 => "recordE$:"},
             {}); 
check_contents(join("$:", qw(recordA recordB recordE 
                             recordZ record4 record5 record6 record7)) . "$:");
# (73-75) NOW we are out of deferred writing mode
$a[0] = "recordF";
check_caches({3 => "recordZ$:", 2 => "recordE$:", 0 => "recordF$:"},
             {}); 
check_contents(join("$:", qw(recordF recordB recordE 
                             recordZ record4 record5 record6 record7)) . "$:");

# (76-79) Last call--untying the array should flush the deferbuffer
$o->defer;
$a[0] = "flushed";
check_caches({3 => "recordZ$:", 2 => "recordE$:"},
             {0 => "flushed$:" }); 
check_contents(join("$:", qw(recordF recordB recordE 
                             recordZ record4 record5 record6 record7)) . "$:");
undef $o;
untie @a;
# (79) We can't use check_contents any more, because the object is dead
open F, "< $file" or die;
{ local $/ ; $z = <F> }
close F;
my $x = join("$:", qw(flushed recordB recordE 
                      recordZ record4 record5 record6 record7)) . "$:";
if ($z eq $x) {
  print "ok $N\n";
} else {
  my $msg = ctrlfix("expected <$x>, got <$z>");
  print "not ok $N \# $msg\n";
}
$N++;

sub check_autodeferring {
  my ($x) = shift;
  my $a = $o->{autodeferring} ? 'ON' : 'OFF';
  if ($x eq $a) {
    print "ok $N\n";
  } else {
    print "not ok $N \# Autodeferring was $a, expected it to be $x\n";
  }
  $N++;
}


sub check_contents {
  my $x = shift;
#  for (values %{$o->{cache}}) {
#    print "# cache=$_";    
#  }
  
  my $integrity = $o->_check_integrity($file, $ENV{INTEGRITY});
  local *FH = $o->{fh};
  seek FH, 0, SEEK_SET;
  print $integrity ? "ok $N\n" : "not ok $N\n";
  $N++;
  my $a;
  { local $/; $a = <FH> }
  $a = "" unless defined $a;
  if ($a eq $x) {
    print "ok $N\n";
  } else {
    ctrlfix(my $msg = "# expected <$x>, got <$a>");
    print "not ok $N\n$msg\n";
  }
  $N++;
}

sub ctrlfix {
  for (@_) {
    s/\n/\\n/g;
    s/\r/\\r/g;
  }
}

END {
  1 while unlink $file;
}

