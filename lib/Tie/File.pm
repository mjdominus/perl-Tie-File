
package Tie::File;
use Carp;
use Fcntl ':seek', 'O_CREAT', 'O_RDWR';
require 5.005;

$VERSION = 0.02;

# Idea: The object will always contain an array of byte offsets
# this will be filled in as is necessary and convenient.
# fetch will do seek-read.
# There will be a cache parameter that controls the amount of cached *data*
# Also an LRU queue of cached records
# store will read the relevant record into the cache
# If it's the same length as what is being written, it will overwrite it in 
#   place; if not, it will do a from-to copying write.
# The record separator string is also a parameter

# Record numbers start at ZERO.

my $DEFAULT_CACHE_SIZE = 1<<21;    # 2 megabytes

sub TIEARRAY {
  if (@_ % 2 != 0) {
    croak "usage: tie \@array, $_[0], filename, [option => value]...";
  }
  my ($pack, $file, %opts) = @_;

  # transform '-foo' keys into 'foo' keys
  for my $key (keys %opts) {
    my $okey = $key;
    if ($key =~ s/^-+//) {
      $opts{$key} = delete $opts{$okey};
    }
  }

  $opts{cachesize} ||= $DEFAULT_CACHE_SIZE;

  # the cache is a hash instead of an array because it is likely to be
  # sparsely populated
  $opts{cache} = {}; 
  $opts{cached} = 0;   # total size of cached data
  $opts{lru} = [];     # replace with heap in later version

  $opts{offsets} = [0];
  $opts{filename} = $file;
  $opts{recsep} = $/ unless defined $opts{recsep};
  $opts{recseplen} = -length($opts{recsep}); # Trick

  my $mode = defined($opts{mode}) ? $opts{mode} : O_CREAT|O_RDWR;

  my $fh = \do { local *FH };   # only works in 5.005 and later
  sysopen $fh, $file, $mode, 0666 or return;
  { my $ofh = select $fh; $| = 1; select $ofh } # autoflush on write
  $opts{fh} = $fh;

  bless \%opts => $pack;
}

sub FETCH {
  my ($self, $n) = @_;

  # check the record cache
  { my $cached = $self->_check_cache($n);
    return $cached if defined $cached;
  }

  unless ($#{$self->{offsets}} >= $n) {
    my $o = $self->_fill_offsets_to($n);
    # If it's still undefined, there is no such record, so return 'undef'
    return unless defined $o;
  }

  my $fh = $self->{FH};
  $self->_seek($n);             # we can do this now that offsets is populated
  my $rec = $self->_read_record;
  $self->_cache_insert($n, $rec) if defined $rec;
  $rec;
}

sub STORE {
  my ($self, $n, $rec) = @_;

  # If the record does not already end with the appropriate terminator
  # string, append one.  Note that 'recseplen' is *negative* here.
  $rec .= $self->{recsep}
    unless substr($rec, $self->{recseplen}) eq $self->{recsep};

  # TODO: what should we do about the cache?  Install the new record
  # in the cache only if the old version of the same record was
  # already there?

  # We need this to decide whether the new record will fit
  # It incidentally populates the offsets table 
  # Note we have to do this before we alter the cache
  my $oldrec = $self->FETCH($n);

  # _check_cache promotes record $n to MRU.  Is this correct behavior?
  $self->{cache}{$n} = $rec if $self->_check_cache($n);

  if (not defined $oldrec) {
    # We're storing a record beyond the end of the file
    $self->_extend_file_to($n);
    $oldrec = $self->{recsep};
  }

  # The file is now at least n records long; we can overwrite 
  # record n with the new record
  my $reclen = length($rec);
  my $len_diff = $reclen - length($oldrec);

  if ($len_diff == 0) {          # Woo-hoo!
    my $fh = $self->{fh};
    $self->_seek($n);
    $self->_write_record($rec);
    return;                     # well, that was easy.
  } 

  # the two records are of different lengths
  # our strategy here: rewrite the tail of the file,
  # reading ahead one buffer at a time
  # $bufsize is required to be at least as large as the record we're inserting
  my $bufsize = _bufsize($reclen);
  my ($writepos, $readpos) = @{$self->{offsets}}[$n,$n+1];

  while (length $rec > 0) {
    $self->_seekb($readpos);
    my $br = read $self->{fh}, my($nextrec), $bufsize;
    $self->_seekb($writepos);
    $self->_write_record($rec);
    $readpos += $br;
    $writepos += length $rec;
    $rec = $nextrec;
  }

  # There might be leftover data at the end of the file
  $self->_chop_file if $len_diff < 0;

  # now update the offsets
  # array slice goes from element $n+1 (the first one to move)
  # to the end
  for (@{$self->{offsets}}[$n+1 .. $#{$self->{offsets}}]) {
    $_ += $len_diff;
  }

  1;
}

sub FETCHSIZE {
  my $self = shift;
  my $n = $#{$self->{offsets}};
  while (defined ($self->_fill_offsets_to($n+1))) {
    ++$n;
  }
  $n;
}

sub STORESIZE {
  my ($self, $len) = @_;
  my $olen = $self->FETCHSIZE;
  return if $len == $olen;

  # file gets longer
  if ($len > $olen) {
    $self->_extend_file_to($len-1);  # record numbers from 0 .. $len-1
    return;
  }

  # file gets shorter
  $self->_seek($len);
  $self->_chop_file;
  $#{$self->{offsets}} = $len-1;
  my @cached = grep $_ > $len, keys %{$self->{cache}};
  delete @{$self->{cache}}{@cached} if @cached;
}

# seek to the beginning of record #$n
# Assumes that the offsets table is already correctly populated
# Negative $n are taken to be record counts from the end
# For example, $n=-1 means just past the last record.
sub _seek {
  my ($self, $n) = @_;
  my $o = $self->{offsets}[$n];
  defined($o)
    or confess("logic error: undefined offset for record $n");
  seek $self->{fh}, $o, SEEK_SET
    or die "Couldn't seek filehandle: $!";  # "Should never happen."
}

sub _seekb {
  my ($self, $b) = @_;
  seek $self->{fh}, $b, SEEK_SET
    or die "Couldn't seek filehandle: $!";  # "Should never happen."
}

# populate the offsets table up to the beginning of record $n
# return the offset of record $n
sub _fill_offsets_to {
  my ($self, $n) = @_;
  my $fh = $self->{fh};
  local *OFF = $self->{offsets};
  my $rec;

  until ($#OFF >= $n) {
    my $o = $OFF[-1];
    $self->_seek(-1);           # tricky
    $rec = $self->_read_record;
    if (defined $rec) {
      push @OFF, $o+length($rec);
    } else {
      return;                   # It turns out there is no such record
    }
  }
 
  # we have now read all the records up to record n-1,
  # so we can return the offset of record n
  return $OFF[$n];
}

# assumes that $rec is already suitably terminated
sub _write_record {
  my ($self, $rec) = @_;
  my $fh = $self->{fh};
  print $fh $rec
    or die "Couldn't write record: $!";  # "Should never happen."

}

sub _read_record {
  my $self = shift;
  my $rec;
  { local $/ = $self->{recsep};
    my $fh = $self->{fh};
    $rec = <$fh>;
  }
  $rec;
}

sub _cache_insert {
  my ($self, $n, $rec) = @_;

  # Do not cache records that are too big to fit in the cache.
  return unless length $rec <= $self->{cachesize};

  $self->{cache}{$n} = $rec;
  $self->{cached} += length $rec;
  push @{$self->{lru}}, $n;     # most-recently-used is at the END

  $self->_cache_flush if $self->{cached} > $self->{cachesize};
}

sub _check_cache {
  my ($self, $n) = @_;
  my $rec;
  return unless defined($rec = $self->{cache}{$n});

  # cache hit; update LRU queue and return $rec
  # replace this with a heap in a later version
  @{$self->{lru}} = ((grep $_ ne $n, @{$self->{lru}}), $n);
  $rec;
}

sub _cache_flush {
  my ($self) = @_;
  while ($self->{cached} > $self->{cachesize}) {
    my $lru = pop @{$self->{lru}};
    $self->{cached} -= length $lru;
    delete $self{cache}{$lru};
  }
  # Now either there's only 
}

# We have read to the end of the file and have the offsets table
# entirely populated.  Now we need to write a new record beyond
# the end of the file.  We prepare for this by writing
# empty records into the file up to the position we want
# $n here is the record number of the last record we're going to write
sub _extend_file_to {
  my ($self, $n) = @_;
  $self->_seek(-1);             # position after the end of the last record
  my $pos = $self->{offsets}[-1];

  # the offsets table has one entry more than the total number of records
  $extras = $n - ($#{$self->{offsets}} - 1);

  # Todo : just use $self->{recsep} x $extras here?
  while ($extras-- > 0) {
    $self->_write_record($self->{recsep});
    $pos -= $self->{recseplen}; # because it's negative
    push @{$self->{offsets}}, $pos;
  }
}

# Truncate the file at the current position
sub _chop_file {
  my $self = shift;
  truncate $self->{fh}, tell($self->{fh});
}

# compute the size of a buffer suitable for moving
# all the data in a file forward $n bytes
# ($n may be negative)
# The result should be at least $n.
sub _bufsize {
  my $n = shift;
  return 8192 if $n < 0;
  my $b = $n & ~8191;
  $b += 8192 if $n & 8191;
  $b;
}

=head1 NAME

Tie::File - Access the lines of a disk file via a Perl array

=head1 SYNOPSIS

	# This file documents Tie::File version 0.02

	tie @array, 'Tie::File', filename;

	$array[13] = 'blah';     # line 13 of the file is now 'blah'
	print $array[42];        # display line 42 of the file

	$n_recs = @array;        # how many records are in the file?
	$#array = $n_recs - 2;   # chop records off the end

	untie @array;            # all finished

=head1 DESCRIPTION

C<Tie::File> represents a regular text file as a Perl array.  Each
element in the array corresponds to a record in the file.  The first
line of the file is element 0 of the array; the second line is element
1, and so on.

The file is I<not> loaded into memory, so this will work even for
gigantic files.

Changes to the array are reflected in the file immediately.

=head2 C<recsep>

What is a 'record'?  By default, the meaning is the same as for the
C<E<lt>...E<gt>> operator: It's a string terminated by C<$/>, which is
probably C<"\n"> or C<"\r\n">.  You may change the definition of
"record" by supplying the C<recsep> option in the C<tie> call:

	tie @array, 'Tie::File', $file, recsep => 'es';

This says that records are delimited by the string C<es>.  If the file contained the following data:

	These pesky flies!\n

then the C<@array> would appear to have four elements: 

	"Thes"
	"e pes"
	"ky flies"
	"!\n"

An undefined value is not permitted as a record separator.  Perl's
special "paragraph mode" semantics (E<agrave> la C<$/ = "">) are not
emulated.

Records read from the tied array will have the record separator string
on the end, just as if they were read from the C<E<lt>...E<gt>>
operator.  Records stored into the array will have the record
separator string appended before they are written to the file, if they
don't have one already.  For example, if the record separator string
is C<"\n">, then the following two lines do exactly the same thing:

	$array[17] = "Cherry pie";
	$array[17] = "Cherry pie\n";

The result is that the contents of line 17 of the file will be
replaced with "Cherry pie"; a newline character will separate line 17
from line 18.  There is no way to create a file whose trailing record
separator string is missing.

Inserting records that I<contain> the record separator string will
produce a reasonable result, but if you can't foresee what this result
will be, you'd better avoid doing this.

=head2 C<mode>

Normally, the specified file will be opened for read and write access,
and will be created if it does not exist.  (That is, the flags
C<O_RDWR | O_CREAT> are supplied in the C<open> call.)  If you want to
change this, you may supply alternative flags in the C<mode> option.
See L<Fcntl> for a listing of available flags.
For example:


	# open the file if it exists, but fail if it does not exist
	tie @array, 'Tie::File', $file, mode => O_RDWR;

Opening the data file in read-only, write-only, or append mode is not
supported.

=head2 C<cachesize>

Records read in from the file are cached, to avoid having to re-read
them repeatedly.  If you read the same record twice, the first time it
will be stored in memory, and the second time it will be fetched from
memory.

The cache has a bounded size; when it exceeds this size, the
least-recently visited records will be purged from the cache.  The
default size is 2Mib.  You can adjust the amount of space used for the
cache by supplying the C<cachesize> option.  The argument is the desired cache size, in bytes.

	# I have a lot of memory, so use a large cache to speed up access
	tie @array, 'Tie::File', $file, cachesize => 20_000_000;

Setting the cache size to 0 will inhibit caching; records will be
fetched from disk every time you examine them.

=head1 Public Methods

The C<tie> call returns an object, say C<$o>.  You may call 

	$rec = $o->FETCH($n);
	$o->STORE($n, $rec);

to fetch or store the record at line C<$n>, respectively.  There are
no other public methods in this package.

=head1 CAVEATS

(That's Latin for 'warnings'.)

=head2 Efficiency Note

Every effort was made to make this module efficient.  Nevertheless,
inserting a long record in the middle of a large file will always be
slow, because everytihing after the new record must be copied forward
towards the end of the file.

In particular, note that:

	# million-line file
	for (@file_array) {
	  $_ .= 'x';
	}

is likely to be very slow, because the first iteration must relocate
lines 1 through 999,999; the second iteration must relocate lines 2
through 999,999, and so on.  The relocation is done using block
writes, however, so it's not as slow as it might be.

A future version of this module will provide some mechanism for
getting better performance in such cases, by deferring the writing
until it can be done all at once.

=head2 Efficiency Note 2

Not every effort was made to make this module as efficient as
possible.  C<FETCHSIZE> should use binary search instead of linear
search.  The cache's LRU queue should be a heap instead of a list.
These defects are probably minor; in any event, they will be fixed in
a later version of the module.

=head2 Missing Methods

The tied array does not yet support C<push>, C<pop>, C<shift>,
C<unshift>, C<splice>, or size-setting via C<$#array = $n>.  I will
put these in soon.

=head1 AUTHOR

Mark Jason Dominus

To contact the author, send email to: C<mjd-perl-tiefile+@plover.com>

To receive an announcement whenever a new version of this module is
released, send a blank email message to
C<mjd-perl-tiefile-subscribe@plover.com>.

=head1 LICENSE

C<Tie::File> version 0.02 is copyright (C) 2002 Mark Jason Dominus.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; it should be in the file C<COPYING>.  If not,
write to the Free Software Foundation, Inc., 59 Temple Place, Suite
330, Boston, MA 02111 USA

For licensing inquiries, contact the author at:

	Mark Jason Dominus
	255 S. Warnock St.
	Philadelphia, PA 19107

=head1 WARRANTY

C<Tie::File> version 0.02 comes with ABSOLUTELY NO WARRANTY.
For details, see the license.

=head1 TODO

C<push>, C<pop>, C<shift>, C<unshift>, C<splice>.

More tests.  (Configuration options, cache flushery, alternative
record separators.)

More tests.  (Stuff I didn't think of yet.)

File locking.

Deferred writing.

Paragraph mode?

More tests.

=cut

