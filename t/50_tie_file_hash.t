#!/usr/bin/perl

use Test::More;
use Test::Deep;
use Tie::File::Hash;
use Fcntl;

plan tests => 21;
my $file = "tf$$.txt";

{
    ok(tie %f, 'Tie::File::Hash', $file, { mode => O_RDWR | O_CREAT });
    for (1..3) {
	# Insert new records
	$f{$_} = $_ * $_;
    }
    for (1..3) {
	# mutate old records
	is($f{$_}, $_ * $_);
	$f{$_} = $_ + 1;
    }
    for (1..3) {
	is($f{$_}, $_ + 1);
    }
}
untie %f;

check_file("1: 2\n", "2: 3\n", "3: 4\n");

{
    ok(tie %f, 'Tie::File::Hash', $file, { mode => O_RDWR | O_CREAT });
    for (3, 2) {
	# mutate old records
	is($f{$_}, $_ + 1);
	$f{$_} = "foo$_";
    }
    $f{"11"} = "eleven";
    is ($f{1}, 2);
    is ($f{11}, "eleven");
    is ($f{2}, "foo2");
}

# Delete
{
  delete $f{2};
  is ($f{2}, undef);
  is ($f{11}, "eleven");
  check_file("1: 2\n", "3: foo3\n", "11: eleven\n");
  $f{2} = "two";
  is ($f{1}, "2");
  is ($f{2}, "two");
  is ($f{3}, "foo3");
  check_file("1: 2\n", "3: foo3\n", "11: eleven\n", "2: two\n");
}

sub check_file {
  my (@x) = @_;
  my @a = do { open my ($fh), "<", $file or die; <$fh> };
  is_deeply(\@a, \@x, "raw file data");
}

END { untie %f; 1 while unlink $file }
