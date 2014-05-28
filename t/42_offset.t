#!/usr/bin/perl

# 2003-04-09 Tels: test the offset method from 0.94

use strict;
use File::Spec;
sub is ($$;$) ;

use POSIX 'SEEK_SET';
my $file = "tf$$.txt";
my $N = 1;

BEGIN
  {
  $| = 1;
  unshift @INC, File::Spec->catdir(File::Spec->updir, 'lib');
  chdir 't' if -d 't';
  print "# INC = @INC\n";

  print "1..24\n";

  }

use Tie::File;
is(1,1,"Partial credit for showing up.");

sub is ($$;$) {
  my ($a, $x, $r) = @_;
  if (! defined $x) {
    if (! defined $a) {
      $a = $x = 'undef';
    } else {
      $x = "undef";
      $a = "'$a'";
    }
  } elsif  (! defined $a) {
    $a = "undef";
    $x = "'$x'";
  }

  if ($a eq $x) { print "ok $N\n" 
  } else { 
    if ($r eq "") {
      print "not ok $N  # expected $x, got $a\n";
    } else {
      print "not ok $N  # $r: expected $x, got $a\n";
    }
  }
  $N++;
}

$/ = "#";	# avoid problems with \n\r vs. \n

my @a;
my $o = tie @a, 'Tie::File', $file, autodefer => 0;

is (ref($o), 'Tie::File');

is ($o->offset(0), 0, 'first one always there');
is ($o->offset(1), undef, 'no offsets yet');

$a[0] = 'Bourbon';
is ($o->offset(0), 0, 'first is ok');
is ($o->offset(1), 8, 'and second ok');
is ($o->offset(2), undef, 'third undef');

$a[1] = 'makes';
is ($o->offset(0), 0, 'first is ok');
is ($o->offset(1), 8, 'and second ok');
is ($o->offset(2), 14, 'and third ok');
is ($o->offset(3), undef, 'fourth undef');

$a[2] = 'the baby';
is ($o->offset(0), 0, 'first is ok');
is ($o->offset(1), 8, 'and second ok');
is ($o->offset(2), 14, 'and third ok');
is ($o->offset(3), 23, 'and fourth ok');
is ($o->offset(4), undef, 'fourth undef');

$a[3] = 'grin';
is ($o->offset(0), 0, 'first is ok');
is ($o->offset(1), 8, 'and second ok');
is ($o->offset(2), 14, 'and third ok');
is ($o->offset(3), 23, 'and fourth ok');
is ($o->offset(4), 28, 'and fifth ok');

$a[4] = '!';
is ($o->offset(5), 30, 'and fifth ok');
$a[3] = 'water';
is ($o->offset(4), 29, 'and fourth changed ok');
is ($o->offset(5), 31, 'and fifth ok');

END {
  undef $o;
  untie @a;
  1 while unlink $file;
}
