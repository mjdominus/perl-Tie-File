#!/usr/bin/perl
#
# Check SPLICE function's effect on the file
# (07_rv_splice.t checks its return value)
#
# Each call to 'check_contents' actually performs two tests.
# First, it calls the tied object's own 'check_integrity' method,
# which makes sure that the contents of the read cache and offset tables
# accurately reflect the contents of the file.  
# Then, it checks the actual contents of the file against the expected
# contents.

use lib '/home/mjd/src/perl/Tie-File2/lib';
my $file = "tf$$.txt";
my $data = "rec0\nrec1\nrec2\n";

print "1..88\n";

my $N = 1;
use Tie::File;
print "ok $N\n"; $N++;  # partial credit just for showing up

my $o = tie @a, 'Tie::File', $file;
print $o ? "ok $N\n" : "not ok $N\n";
$N++;

my $n;

# (3-22) splicing at the beginning
init_file($data);

splice(@a, 0, 0, "rec4");
check_contents("rec4\n$data");
splice(@a, 0, 1, "rec5");       # same length
check_contents("rec5\n$data");
splice(@a, 0, 1, "record5");    # longer
check_contents("record5\n$data");

splice(@a, 0, 1, "r5");         # shorter
check_contents("r5\n$data");
splice(@a, 0, 1);               # removal
check_contents("$data");
splice(@a, 0, 0);               # no-op
check_contents("$data");
splice(@a, 0, 0, 'r7', 'rec8'); # insert more than one
check_contents("r7\nrec8\n$data");
splice(@a, 0, 2, 'rec7', 'record8', 'rec9'); # insert more than delete
check_contents("rec7\nrecord8\nrec9\n$data");

splice(@a, 0, 3, 'record9', 'rec10'); # delete more than insert
check_contents("record9\nrec10\n$data");
splice(@a, 0, 2);               # delete more than one
check_contents("$data");


# (23-42) splicing in the middle
splice(@a, 1, 0, "rec4");
check_contents("rec0\nrec4\nrec1\nrec2\n");
splice(@a, 1, 1, "rec5");       # same length
check_contents("rec0\nrec5\nrec1\nrec2\n");
splice(@a, 1, 1, "record5");    # longer
check_contents("rec0\nrecord5\nrec1\nrec2\n");

splice(@a, 1, 1, "r5");         # shorter
check_contents("rec0\nr5\nrec1\nrec2\n");
splice(@a, 1, 1);               # removal
check_contents("$data");
splice(@a, 1, 0);               # no-op
check_contents("$data");
splice(@a, 1, 0, 'r7', 'rec8'); # insert more than one
check_contents("rec0\nr7\nrec8\nrec1\nrec2\n");
splice(@a, 1, 2, 'rec7', 'record8', 'rec9'); # insert more than delete
check_contents("rec0\nrec7\nrecord8\nrec9\nrec1\nrec2\n");

splice(@a, 1, 3, 'record9', 'rec10'); # delete more than insert
check_contents("rec0\nrecord9\nrec10\nrec1\nrec2\n");
splice(@a, 1, 2);               # delete more than one
check_contents("$data");

# (43-62) splicing at the end
splice(@a, 3, 0, "rec4");
check_contents("$ {data}rec4\n");
splice(@a, 3, 1, "rec5");       # same length
check_contents("$ {data}rec5\n");
splice(@a, 3, 1, "record5");    # longer
check_contents("$ {data}record5\n");

splice(@a, 3, 1, "r5");         # shorter
check_contents("$ {data}r5\n");
splice(@a, 3, 1);               # removal
check_contents("$data");
splice(@a, 3, 0);               # no-op
check_contents("$data");
splice(@a, 3, 0, 'r7', 'rec8'); # insert more than one
check_contents("$ {data}r7\nrec8\n");
splice(@a, 3, 2, 'rec7', 'record8', 'rec9'); # insert more than delete
check_contents("$ {data}rec7\nrecord8\nrec9\n");

splice(@a, 3, 3, 'record9', 'rec10'); # delete more than insert
check_contents("$ {data}record9\nrec10\n");
splice(@a, 3, 2);               # delete more than one
check_contents("$data");

# (63-82) splicing with negative subscript
splice(@a, -1, 0, "rec4");
check_contents("rec0\nrec1\nrec4\nrec2\n");
splice(@a, -1, 1, "rec5");       # same length
check_contents("rec0\nrec1\nrec4\nrec5\n");
splice(@a, -1, 1, "record5");    # longer
check_contents("rec0\nrec1\nrec4\nrecord5\n");

splice(@a, -1, 1, "r5");         # shorter
check_contents("rec0\nrec1\nrec4\nr5\n");
splice(@a, -1, 1);               # removal
check_contents("rec0\nrec1\nrec4\n");
splice(@a, -1, 0);               # no-op  
check_contents("rec0\nrec1\nrec4\n");
splice(@a, -1, 0, 'r7', 'rec8'); # insert more than one
check_contents("rec0\nrec1\nr7\nrec8\nrec4\n");
splice(@a, -1, 2, 'rec7', 'record8', 'rec9'); # insert more than delete
check_contents("rec0\nrec1\nr7\nrec8\nrec7\nrecord8\nrec9\n");

splice(@a, -3, 3, 'record9', 'rec10'); # delete more than insert
check_contents("rec0\nrec1\nr7\nrec8\nrecord9\nrec10\n");
splice(@a, -4, 3);               # delete more than one
check_contents("rec0\nrec1\nrec10\n");

# (83-84) scrub it all out
splice(@a, 0, 3);
check_contents("");

# (85-86) put some back in
splice(@a, 0, 0, "rec0", "rec1");
check_contents("rec0\nrec1\n");

# (87-88) what if we remove too many records?
splice(@a, 0, 17);
check_contents("");

sub init_file {
  my $data = shift;
  open F, "> $file" or die $!;
  print F $data;
  close F;
}

sub check_contents {
  my $x = shift;
  local *FH;
  my $integrity = $o->_check_integrity($file, $ENV{INTEGRITY});
  print $integrity ? "ok $N\n" : "not ok $N\n";
  $N++;
  my $open = open FH, "< $file";
  my $a;
  { local $/; $a = <FH> }
  print (($open && $a eq $x) ? "ok $N\n" : "not ok $N\n");
  $N++;
}

END {
  1 while unlink $file;
}

