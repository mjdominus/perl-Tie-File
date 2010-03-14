#!/usr/bin/perl
#
# Check FETCHSIZE and SETSIZE functions
# PUSH POP SHIFT UNSHIFT
#

my $file = "tf$$.txt";
my $data = "rec0blahrec1blahrec2blah";
my ($o, $n);

print "1..10\n";

my $N = 1;
use Tie::File;
print "ok $N\n"; $N++;

# 2-3 FETCHSIZE 0-length file
open F, "> $file" or die $!;
close F;
$o = tie @a, 'Tie::File', $file, recsep => 'blah';
print $o ? "ok $N\n" : "not ok $N\n";
$N++;
$n = @a;
print $n == 0 ? "ok $N\n" : "not ok $N # $n, s/b 0\n";
$N++;

# Reset everything
undef $o;
untie @a;

# 4-5 FETCHSIZE positive-length file
open F, "> $file" or die $!;
print F $data;
close F;
$o = tie @a, 'Tie::File', $file, recsep => 'blah';
print $o ? "ok $N\n" : "not ok $N\n";
$N++;
$n = @a;
print $n == 3 ? "ok $N\n" : "not ok $N # $n, s/b 0\n";
$N++;

# STORESIZE
# 6 Make it longer:
$#a = 4;
check_contents("${data}blahblah");

# 7 Make it longer again:
$#a = 6;
check_contents("${data}blahblahblahblah");

# 8 Make it shorter:
$#a = 4;
check_contents("${data}blahblah");

# 9 Make it shorter again:
$#a = 2;
check_contents($data);

# 10 Get rid of it completely:
$#a = -1;
check_contents('');


sub check_contents {
  my $x = shift;
  local *FH;
  my $open = open FH, "< $file";
  my $a;
  { local $/; $a = <FH> }
  print (($open && $a eq $x) ? "ok $N\n" : "not ok $N\n");
  $N++;
}


END {
  1 while unlink $file;
}
