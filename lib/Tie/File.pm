
package Tie::File;
require 5.005;
use Carp;
use POSIX 'SEEK_SET';
use Fcntl 'O_CREAT', 'O_RDWR', 'LOCK_EX';

$VERSION = "0.52";
my $DEFAULT_MEMORY_SIZE = 1<<21;    # 2 megabytes
my $DEFAULT_AUTODEFER_THRESHHOLD = 3; # 3 records
my $DEFAULT_AUTODEFER_FILELEN_THRESHHOLD = 65536; # 16 disk blocksful

my %good_opt = map {$_ => 1, "-$_" => 1}
  qw(memory mode recsep discipline autodefer autochomp);

sub TIEARRAY {
  if (@_ % 2 != 0) {
    croak "usage: tie \@array, $_[0], filename, [option => value]...";
  }
  my ($pack, $file, %opts) = @_;

  # transform '-foo' keys into 'foo' keys
  for my $key (keys %opts) {
    unless ($good_opt{$key}) {
      croak("$pack: Unrecognized option '$key'\n");
    }
    my $okey = $key;
    if ($key =~ s/^-+//) {
      $opts{$key} = delete $opts{$okey};
    }
  }

  $opts{memory} = $DEFAULT_MEMORY_SIZE unless defined $opts{memory};

  # are we in deferred-write mode?
  $opts{defer} = 0 unless defined $opts{defer};
  $opts{deferred} = {};         # no records are presently deferred

  # the cache is a hash instead of an array because it is likely to be
  # sparsely populated
  $opts{cache} = {};
  $opts{cached} = 0;   # total size of cached data
  $opts{lru} = [];

  # autodeferment is enabled by default
  $opts{autodefer} = 1 unless defined $opts{autodefer};
  $opts{autodeferring} = 0;     # but is not initially active
  $opts{ad_history} = [];
  $opts{autodefer_threshhold} = $DEFAULT_AUTODEFER_THRESHHOLD
    unless defined $opts{autodefer_threshhold};
  $opts{autodefer_filelen_threshhold} = $DEFAULT_AUTODEFER_FILELEN_THRESHHOLD
    unless defined $opts{autodefer_filelen_threshhold};

  $opts{offsets} = [0];
  $opts{filename} = $file;
  unless (defined $opts{recsep}) { 
    $opts{recsep} = _default_recsep();
  }
  $opts{recseplen} = length($opts{recsep});
  if ($opts{recseplen} == 0) {
    croak "Empty record separator not supported by $pack";
  }

  $opts{autochomp} = 1 unless defined $opts{autochomp};

  my $mode = defined($opts{mode}) ? $opts{mode} : O_CREAT|O_RDWR;
  my $fh;

  if (UNIVERSAL::isa($file, 'GLOB')) {
    # We use 1 here on the theory that some systems 
    # may not indicate failure if we use 0.
    # MSWin32 does not indicate failure with 0, but I don't know if
    # it will indicate failure with 1 or not.
    unless (seek $file, 1, SEEK_SET) {
      croak "$pack: your filehandle does not appear to be seekable";
    }
    seek $file, 0, SEEK_SET     # put it back
    $fh = $file;                # setting binmode is the user's problem
  } elsif (ref $file) {
    croak "usage: tie \@array, $pack, filename, [option => value]...";
  } else {
    $fh = \do { local *FH };   # only works in 5.005 and later
    sysopen $fh, $file, $mode, 0666 or return;
    binmode $fh;
  }
  { my $ofh = select $fh; $| = 1; select $ofh } # autoflush on write
  if (defined $opts{discipline} && $] >= 5.006) {
    # This avoids a compile-time warning under 5.005
    eval 'binmode($fh, $opts{discipline})';
    croak $@ if $@ =~ /unknown discipline/i;
    die if $@;
  }
  $opts{fh} = $fh;

  bless \%opts => $pack;
}

sub FETCH {
  my ($self, $n) = @_;

  my $rec = $self->_fetch($n);
  $self->_chomp1($rec);
}

# Chomp many records in-place; return nothing useful
sub _chomp {
  my $self = shift;
  return unless $self->{autochomp};
  if ($self->{autochomp}) {
    for (@_) {
      next unless defined;
      substr($_, - $self->{recseplen}) = "";
    }
  }
}

# Chomp one record in-place; return modified record
sub _chomp1 {
  my ($self, $rec) = @_;
  return $rec unless $self->{autochomp};
  return unless defined $rec;
  substr($rec, - $self->{recseplen}) = "";
  $rec;
}

# This is a true fetch; it always looks in the file  FIX XXX BUG (COMMENT)
sub _fetch {
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

# If we happen to have just read the first record, check to see if
# the length of the record matches what 'tell' says.  If not, Tie::File
# won't work, and should drop dead.
#
#  if ($n == 0 && defined($rec) && tell($self->{fh}) != length($rec)) {
#    if (defined $self->{discipline}) {
#      croak "I/O discipline $self->{discipline} not supported";
#    } else {
#      croak "File encoding not supported";
#    }
#  }

  $self->_cache_insert($n, $rec) if defined $rec && not $self->{flushing};
  $rec;
}

sub STORE {
  my ($self, $n, $rec) = @_;
  die "STORE called from _check_integrity!" if $DIAGNOSTIC;

  $self->_fixrecs($rec);

  if ($self->{autodefer}) {
    $self->_annotate_ad_history($n);
  }

  return $self->_store_deferred($n, $rec) if $self->_is_deferring;


  # We need this to decide whether the new record will fit
  # It incidentally populates the offsets table 
  # Note we have to do this before we alter the cache
  # 20020324 Wait, but this DOES alter the cache.  TODO BUG?
  my $oldrec = $self->_fetch($n);

  if (my $cached = $self->_check_cache($n)) {
    my $len_diff = length($rec) - length($cached);
    $self->{cache}{$n} = $rec;
    $self->{cached} += $len_diff;
    $self->_cache_flush if $len_diff > 0 && $self->_cache_too_full;
  }

  if (not defined $oldrec) {
    # We're storing a record beyond the end of the file
    $self->_extend_file_to($n+1);
    $oldrec = $self->{recsep};
  }
  my $len_diff = length($rec) - length($oldrec);

  # length($oldrec) here is not consistent with text mode  TODO XXX BUG
  $self->_twrite($rec, $self->{offsets}[$n], length($oldrec));

  # now update the offsets
  # array slice goes from element $n+1 (the first one to move)
  # to the end
  for (@{$self->{offsets}}[$n+1 .. $#{$self->{offsets}}]) {
    $_ += $len_diff;
  }
}

sub _store_deferred {
  my ($self, $n, $rec) = @_;
  $self->{deferred}{$n} = 1;    # this must come first
  $self->_uncache($n);
  $self->_cache_insert($n, $rec);
}

# Remove a single record from the deferred-write buffer without writing it
# The record need not be present
sub _delete_deferred {
  my ($self, $n) = @_;
  delete $self->{deferred}{$n};
}

sub FETCHSIZE {
  my $self = shift;
  my $n = $#{$self->{offsets}};
  # 20020317 Change this to binary search
  while (defined ($self->_fill_offsets_to($n+1))) {
    ++$n;
  }

  # There might be some deferred-write items in the cache
  # whose numbers exceed the actual end of the file
  for my $k (keys %{$self->{cache}}) {
    $n = $k+1 if $n < $k+1;
  }
  $n;
}

sub STORESIZE {
  my ($self, $len) = @_;

  if ($self->{autodefer}) {
    $self->_annotate_ad_history('STORESIZE');
  }

  my $olen = $self->FETCHSIZE;
  return if $len == $olen;      # Woo-hoo!

  # file gets longer
  if ($len > $olen) {
    if ($self->_is_deferring) {
      for ($olen .. $len-1) {
        $self->_store_deferred($_, $self->{recsep});
      }
    } else {
      $self->_extend_file_to($len);
    }
    return;
  }

  # file gets shorter
  if ($self->_is_deferring) {
    # TODO maybe replace this with map-plus-assignment?
    for (grep $_ >= $len, keys %{$self->{deferred}}) {
      $self->_delete_deferred($_);
    }
  }

  $self->_seek($len);
  $self->_chop_file;
  $#{$self->{offsets}} = $len;
#  $self->{offsets}[0] = 0;      # in case we just chopped this
  my @cached = grep $_ >= $len, keys %{$self->{cache}};
  $self->_uncache(@cached);
}

sub PUSH {
  my $self = shift;
  $self->SPLICE($self->FETCHSIZE, scalar(@_), @_);
#  $self->FETCHSIZE;  # av.c takes care of this for me
}

sub POP {
  my $self = shift;
  my $size = $self->FETCHSIZE;
  return if $size == 0;
#  print STDERR "# POPPITY POP POP POP\n";
  scalar $self->SPLICE($size-1, 1);
}

sub SHIFT {
  my $self = shift;
  scalar $self->SPLICE(0, 1);
}

sub UNSHIFT {
  my $self = shift;
  $self->SPLICE(0, 0, @_);
  # $self->FETCHSIZE; # av.c takes care of this for me
}

sub CLEAR {
  my $self = shift;

  if ($self->{autodefer}) {
    $self->_annotate_ad_history('CLEAR');
  }

  $self->_seekb(0);
  $self->_chop_file;
  %{$self->{cache}}   = ();
    $self->{cached}   = 0;
  @{$self->{lru}}     = ();
  @{$self->{offsets}} = (0);
  %{$self->{deferred}}= ();
}

sub EXTEND {
  my ($self, $n) = @_;

  # No need to pre-extend anything in this case
  return if $self->_is_deferring;

  $self->_fill_offsets_to($n);
  $self->_extend_file_to($n);
}

sub DELETE {
  my ($self, $n) = @_;

  if ($self->{autodefer}) {
    $self->_annotate_ad_history('DELETE');
  }

  my $lastrec = $self->FETCHSIZE-1;
  my $rec = $self->FETCH($n);
  $self->_delete_deferred($n) if $self->_is_deferring;
  if ($n == $lastrec) {
    $self->_seek($n);
    $self->_chop_file;
    $#{$self->{offsets}}--;
    $self->_uncache($n);
    # perhaps in this case I should also remove trailing null records?
    # 20020316
    # Note that delete @a[-3..-1] deletes the records in the wrong order,
    # so we only chop the very last one out of the file.  We could repair this
    # by tracking deleted records inside the object.
  } elsif ($n < $lastrec) {
    $self->STORE($n, "");
  }
  $rec;
}

sub EXISTS {
  my ($self, $n) = @_;
  return 1 if exists $self->{cache}{$n};
  $self->_fill_offsets_to($n);  # I think this is unnecessary
  $n < $self->FETCHSIZE;
}

sub SPLICE {
  my $self = shift;

  if ($self->{autodefer}) {
    $self->_annotate_ad_history('SPLICE');
  }

  $self->_flush if $self->_is_deferring; # move this up?
  if (wantarray) {
    $self->_chomp(my @a = $self->_splice(@_));
    @a;
  } else {
    $self->_chomp1(scalar $self->_splice(@_));
  }
}

sub DESTROY {
  my $self = shift;
  $self->flush if $self->_is_deferring;
}

sub _splice {
  my ($self, $pos, $nrecs, @data) = @_;
  my @result;

  $pos = 0 unless defined $pos;

  # Deal with negative and other out-of-range positions
  # Also set default for $nrecs 
  {
    my $oldsize = $self->FETCHSIZE;
    $nrecs = $oldsize unless defined $nrecs;
    my $oldpos = $pos;

    if ($pos < 0) {
      $pos += $oldsize;
      if ($pos < 0) {
        croak "Modification of non-creatable array value attempted, subscript $oldpos";
      }
    }

    if ($pos > $oldsize) {
      return unless @data;
      $pos = $oldsize;          # This is what perl does for normal arrays
    }
  }

  $self->_fixrecs(@data);
  my $data = join '', @data;
  my $datalen = length $data;
  my $oldlen = 0;

  # compute length of data being removed
  for ($pos .. $pos+$nrecs-1) {
    $self->_fill_offsets_to($_);
    my $rec = $self->_fetch($_);
    last unless defined $rec;
    push @result, $rec;

    # Why don't we just use length($rec) here?
    # Because that record might have come from the cache.  _splice
    # might have been called to flush out the deferred-write records,
    # and in this case length($rec) is the length of the record to be *written*,
    # not the length of the actual record in the file.  But the offsets are
    # still true. 20020322
    $oldlen += $self->{offsets}[$_+1] - $self->{offsets}[$_]
      if defined $self->{offsets}[$_+1];
  }

  # Modify the file
  $self->_twrite($data, $self->{offsets}[$pos], $oldlen);

  # update the offsets table part 1
  # compute the offsets of the new records:
  my @new_offsets;
  if (@data) {
    push @new_offsets, $self->{offsets}[$pos];
    for (0 .. $#data-1) {
      push @new_offsets, $new_offsets[-1] + length($data[$_]);
    }
  }
  splice(@{$self->{offsets}}, $pos, $nrecs, @new_offsets);

  # update the offsets table part 2
  # adjust the offsets of the following old records
  for ($pos+@data .. $#{$self->{offsets}}) {
    $self->{offsets}[$_] += $datalen - $oldlen;
  }
  # If we scrubbed out all known offsets, regenerate the trivial table
  # that knows that the file does indeed start at 0.
  $self->{offsets}[0] = 0 unless @{$self->{offsets}};

  # Perhaps the following cache foolery could be factored out
  # into a bunch of mor opaque cache functions.  For example,
  # it's odd to delete a record from the cache and then remove
  # it from the LRU queue later on; there should be a function to
  # do both at once.

  # update the read cache, part 1
  # modified records
  # Consider this carefully for correctness
  for ($pos .. $pos+$nrecs-1) {
    my $cached = $self->{cache}{$_};
    next unless defined $cached;
    my $new = $data[$_-$pos];
    if (defined $new) {
      # update()
      $self->{cached} += length($new) - length($cached);
      $self->{cache}{$_} = $new;
    } else {
      $self->_uncache($_);
    }
  }
  # update the read cache, part 2
  # moved records - records past the site of the change
  # need to be renumbered
  # Maybe merge this with the previous block?
  {
    my %adjusted;
    for (keys %{$self->{cache}}) {
      next unless $_ >= $pos + $nrecs;
      $adjusted{$_-$nrecs+@data} = delete $self->{cache}{$_};
    }
    @{$self->{cache}}{keys %adjusted} = values %adjusted;
    # No need to adjust $self->{deferred} because it should be empty here
#    for (keys %{$self->{cache}}) {
#      next unless $_ >= $pos + $nrecs;
#      $self->{cache}{$_-$nrecs+@data} = delete $self->{cache}{$_};
#    }
  }

  # fix the LRU queue
  my(@new, @changed);
  for (@{$self->{lru}}) {
    if ($_ >= $pos + $nrecs) {
      push @new, $_ + @data - $nrecs;
    } elsif ($_ >= $pos) {
      push @changed, $_ if $_ < $pos + @data;
    } else {
      push @new, $_;
    }
  }
  @{$self->{lru}} = (@new, @changed);

  # Now there might be too much data in the cache, if we spliced out
  # some short records and spliced in some long ones.  If so, flush
  # the cache.
  $self->_cache_flush unless $self->{flushing};

  # Yes, the return value of 'splice' *is* actually this complicated
  wantarray ? @result : @result ? $result[-1] : undef;
}

# write data into the file
# $data is the data to be written. 
# it should be written at position $pos, and should overwrite
# exactly $len of the following bytes.  
# Note that if length($data) > $len, the subsequent bytes will have to 
# be moved up, and if length($data) < $len, they will have to
# be moved down
sub _twrite {
  my ($self, $data, $pos, $len) = @_;

  unless (defined $pos) {
    die "\$pos was undefined in _twrite";
  }

  my $len_diff = length($data) - $len;

  if ($len_diff == 0) {          # Woo-hoo!
    my $fh = $self->{fh};
    $self->_seekb($pos);
    $self->_write_record($data);
    return;                     # well, that was easy.
  }

  # the two records are of different lengths
  # our strategy here: rewrite the tail of the file,
  # reading ahead one buffer at a time
  # $bufsize is required to be at least as large as the data we're overwriting
  my $bufsize = _bufsize($len_diff);
  my ($writepos, $readpos) = ($pos, $pos+$len);
  my $next_block;
  my $more_data;

  # Seems like there ought to be a way to avoid the repeated code
  # and the special case here.  The read(1) is also a little weird.
  # Think about this.
  do {
    $self->_seekb($readpos);
    my $br = read $self->{fh}, $next_block, $bufsize;
    $more_data = read $self->{fh}, my($dummy), 1;
    $self->_seekb($writepos);
    $self->_write_record($data);
    $readpos += $br;
    $writepos += length $data;
    $data = $next_block;
  } while $more_data;           # BUG XXX TODO how could this have worked?
  $self->_seekb($writepos);
  $self->_write_record($next_block);

  # There might be leftover data at the end of the file
  $self->_chop_file if $len_diff < 0;
}

# If a record does not already end with the appropriate terminator
# string, append one.
sub _fixrecs {
  my $self = shift;
  for (@_) {
    $_ .= $self->{recsep}
      unless substr($_, - $self->{recseplen}) eq $self->{recsep};
  }
}


################################################################
#
# Basic read, write, and seek
#

# seek to the beginning of record #$n
# Assumes that the offsets table is already correctly populated
#
# Note that $n=-1 has a special meaning here: It means the start of
# the last known record; this may or may not be the very last record
# in the file, depending on whether the offsets table is fully populated.
#
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
    $self->_seek(-1);           # tricky -- see comment at _seek
    $rec = $self->_read_record;
    if (defined $rec) {
      push @OFF, tell $fh;
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
  $self->{_written} += length($rec);
}

sub _read_record {
  my $self = shift;
  my $rec;
  { local $/ = $self->{recsep};
    my $fh = $self->{fh};
    $rec = <$fh>;
  }
  $self->{_read} += length($rec) if defined $rec;
  $rec;
}

sub _rw_stats {
  @{$self}{'_read', '_written'};
}

################################################################
#
# Read cache management

# Insert a record into the cache at position $n
# Only appropriate when no data is cached for $n already
sub _cache_insert {
  my ($self, $n, $rec) = @_;

  # Do not cache records that are too big to fit in the cache.
  return unless length $rec <= $self->{memory};

  # Do not cache records that have modified versions waiting
  # in the deferred-write buffer
  #  return if exists $self->{deferred}{$n};
  # If you enable this, replace 30_defer.t with 30-defer_alt.t.

  $self->{cache}{$n} = $rec;
  $self->{cached} += length $rec;
  push @{$self->{lru}}, $n;     # most-recently-used is at the END

  $self->_cache_flush if $self->_cache_too_full;
}

# Remove cached data for record $n, if there is any
# (It is OK if $n is not in the cache at all)
sub _uncache {
  my $self = shift;
  for my $n (@_) {
    my $cached = delete $self->{cache}{$n};
    next unless defined $cached;
    @{$self->{lru}} = grep $_ != $n, @{$self->{lru}};
    $self->{cached} -= length($cached);
  }
}

# _check_cache promotes record $n to MRU.  Is this correct behavior?
sub _check_cache {
  my ($self, $n) = @_;
  my $rec;
  return unless defined($rec = $self->{cache}{$n});

  # cache hit; update LRU queue and return $rec
  # replace this with a heap in a later version
  # 20020317 This should be a separate method
  @{$self->{lru}} = ((grep $_ ne $n, @{$self->{lru}}), $n);
  $rec;
}

sub _cache_too_full {
  my $self = shift;
  $self->{cached} > $self->{memory};
}

# TODO REWRITE ME
sub _cache_flush {
  my ($self) = @_;
  return if $self->{flushing};
  local $self->{flushing} = 1;  # prevent infinite recursion
  while ($self->_cache_too_full && %{$self->{cache}}) {

    my ($recno, $lru_index) = $self->_oldest_cached_record;
    if (defined $recno) {
      splice @{$self->{lru}}, $lru_index, 1;
      my $rec = delete $self->{cache}{$recno};
      $self->{cached} -= length $rec;
    } else {                    # last resort
      $self->_flush;            # flush deferred writes
      # Now we'll go back and expire the oldest record as usual
    }
  }
}

sub _oldest_cached_record {
  my $self = shift;
  for (0 .. $#{$self->{lru}}) {
    my $recno = $self->{lru}[$_];
    if (not $self->{deferred}{$recno}) {
      return wantarray ? ($recno, $_) : $recno;
    }
  }
  return;
}

################################################################
#
# File custodial services
#


# We have read to the end of the file and have the offsets table
# entirely populated.  Now we need to write a new record beyond
# the end of the file.  We prepare for this by writing
# empty records into the file up to the position we want
#
# assumes that the offsets table already contains the offset of record $n,
# if it exists, and extends to the end of the file if not.
sub _extend_file_to {
  my ($self, $n) = @_;
  $self->_seek(-1);             # position after the end of the last record
  my $pos = $self->{offsets}[-1];

  # the offsets table has one entry more than the total number of records
  my $extras = $n - $#{$self->{offsets}};

  # Todo : just use $self->{recsep} x $extras here?
  while ($extras-- > 0) {
    $self->_write_record($self->{recsep});
    push @{$self->{offsets}}, tell $self->{fh};
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

################################################################
#
# Miscellaneous public methods
#

# Lock the file
sub flock {
  my ($self, $op) = @_;
  unless (@_ <= 3) {
    my $pack = ref $self;
    croak "Usage: $pack\->flock([OPERATION])";
  }
  my $fh = $self->{fh};
  $op = LOCK_EX unless defined $op;
  flock $fh, $op;
}

# Get/set autochomp option
sub autochomp {
  my $self = shift;
  if (@_) {
    my $old = $self->{autochomp};
    $self->{autochomp} = shift;
    $old;
  } else {
    $self->{autochomp};
  }
}

################################################################
#
# Matters related to deferred writing
#

# Defer writes
sub defer {
  my $self = shift;
  $self->_stop_autodeferring;
  @{$self->{ad_history}} = ();
  $self->{defer} = 1;
}

# Flush deferred writes
#
# This could be better optimized to write the file in one pass, instead
# of one pass per block of records.  But that will require modifications
# to _twrite, so I should have a good _twite test suite first.
sub flush {
  my $self = shift;

  $self->_flush;
  $self->{defer} = 0;
}

sub _flush {
  my $self = shift;
  my @writable = sort {$a<=>$b} (keys %{$self->{deferred}});
  my $w = @writable;

  while (@writable) {
    # gather all consecutive records from the front of @writable
    my $first_rec = shift @writable;
    my $last_rec = $first_rec+1;
    ++$last_rec, shift @writable while @writable && $last_rec == $writable[0];
    --$last_rec;
    $self->_fill_offsets_to($last_rec);
    $self->_extend_file_to($last_rec);
    $self->_splice($first_rec, $last_rec-$first_rec+1, 
                   @{$self->{cache}}{$first_rec .. $last_rec});
    delete @{$self->{deferred}}{$first_rec .. $last_rec};
  }
}

# Discard deferred writes and disable future deferred writes
sub discard {
  my $self = shift;
  $self->_discard;
  $self->{defer} = 0;
}

# Discard deferred writes, but retain old deferred writing mode
sub _discard {
  my $self = shift;
  # Unfortunately we cached the deferred-written records, and now we have
  # to throw those away
  for my $k (keys %{$self->{deferred}}) {
    $self->_uncache($k);
  }
  %{$self->{deferred}} = ();
}

# Deferred writing is enabled, either explicitly ($self->{defer})
# or automatically ($self->{autodeferring})
sub _is_deferring {
  my $self = shift;
  $self->{defer} || $self->{autodeferring};
}

################################################################
#
# Matters related to autodeferment
#

# Get/set autodefer option
sub autodefer {
  my $self = shift;
  if (@_) {
    my $old = $self->{autodefer};
    $self->{autodefer} = shift;
    $old;
  } else {
    $self->{autodefer};
  }
}

# The user is trying to store record #$n Record that in the history,
# and then enable (or disable) autodeferment if that seems useful.
# Note that it's OK for $n to be a non-number, as long as the function
# is prepared to deal with that.  Nobody else looks at the ad_history.
#
# Now, what does the ad_history mean, and what is this function doing?
# Essentially, the idea is to enable autodeferring when we see that the
# user has made three consecutive STORE calls to three consecutive records.
# ("Three" is actually ->{autodefer_threshhold}.)
# A STORE call for record #$n inserts $n into the autodefer history,
# and if the history contains three consecutive records, we enable 
# autodeferment.  An ad_history of [X, Y] means that the most recent
# STOREs were for records X, X+1, ..., Y, in that order.  
#
# Inserting a nonconsecutive number erases the history and starts over.
#
# Performing a special operation like SPLICE erases the history.
#
# There's one special case: CLEAR means that CLEAR was just called.
# In this case, we prime the history with [-2, -1] so that if the next
# write is for record 0, autodeferring goes on immediately.  This is for
# the common special case of "@a = (...)".
#
sub _annotate_ad_history {
  my ($self, $n) = @_;
  return unless $self->{autodefer}; # feature is disabled
  return if $self->{defer};     # already in explicit defer mode
  return unless $self->{offsets}[-1] >= $self->{autodefer_filelen_threshhold};

  local *H = $self->{ad_history};
  if ($n eq 'CLEAR') {
    @H = (-2, -1);              # prime the history with fake records
    $self->_stop_autodeferring;
  } elsif ($n =~ /^\d+$/) {
    if (@H == 0) {
      @H =  ($n, $n);
    } else {                    # @H == 2
      if ($H[1] == $n-1) {      # another consecutive record
        $H[1]++;
        if ($H[1] - $H[0] + 1 >= $self->{autodefer_threshhold}) {
          $self->{autodeferring} = 1;
        }
      } else {                  # nonconsecutive- erase and start over
        @H = ($n, $n);
        $self->_stop_autodeferring;
      }
    }
  } else {                      # SPLICE or STORESIZE or some such
    @H = ();
    $self->_stop_autodeferring;
  }
}

# If autodferring was enabled, cut it out and discard the history
sub _stop_autodeferring {
  my $self = shift;
  if ($self->{autodeferring}) {
    $self->_flush;
  }
  $self->{autodeferring} = 0;
}

################################################################


# This is NOT a method.  It is here for two reasons:
#  1. To factor a fairly complicated block out of the constructor
#  2. To provide access for the test suite, which need to be sure
#     files are being written properly.
sub _default_recsep {
  my $recsep = $/;
  if ($^O eq 'MSWin32') {       # Dos too?
    # Windows users expect files to be terminated with \r\n
    # But $/ is set to \n instead
    # Note that this also transforms \n\n into \r\n\r\n.
    # That is a feature.
    $recsep =~ s/\n/\r\n/g;
  }
  $recsep;
}

# Utility function for _check_integrity
sub _ci_warn {
  my $msg = shift;
  $msg =~ s/\n/\\n/g;
  $msg =~ s/\r/\\r/g;
  print "# $msg\n";
}

# Given a file, make sure the cache is consistent with the
# file contents and the internal data structures are consistent with
# each other.  Returns true if everything checks out, false if not
#
# The $file argument is no longer used.  It is retained for compatibility
# with the existing test suite.
sub _check_integrity {
  my ($self, $file, $warn) = @_;
  my $rsl = $self->{recseplen};
  my $rs  = $self->{recsep};
  my $good = 1; 
  local *_;                     # local $_ does not work here
  local $DIAGNOSTIC = 1;

  if (not defined $rs) {
    _ci_warn("recsep is undef!");
    $good = 0;
  } elsif ($rs eq "") {
    _ci_warn("recsep is empty!");
    $good = 0;
  } elsif ($rsl != length $rs) {
    my $ln = length $rs;
    _ci_warn("recsep <$rs> has length $ln, should be $rsl");
    $good = 0;
  }

  if (not defined $self->{offsets}[0]) {
    _ci_warn("offset 0 is missing!");
    $good = 0;
  } elsif ($self->{offsets}[0] != 0) {
    _ci_warn("rec 0: offset <$self->{offsets}[0]> s/b 0!");
    $good = 0;
  }

  my $cached = 0;
  {
    local *F = $self->{fh};
    seek F, 0, SEEK_SET;
    local $. = 0;
    local $/ = $rs;

    while (<F>) {
      my $n = $. - 1;
      my $cached = $self->{cache}{$n};
      my $offset = $self->{offsets}[$.];
      my $ao = tell F;
      if (defined $offset && $offset != $ao) {
        _ci_warn("rec $n: offset <$offset> actual <$ao>");
        $good = 0;
      }
      if (defined $cached && $_ ne $cached && ! $self->{deferred}{$n}) {
        $good = 0;
        _ci_warn("rec $n: cached <$cached> actual <$_>");
      }
      if (defined $cached && substr($cached, -$rsl) ne $rs) {
        _ci_warn("rec $n in the cache is missing the record separator");
      }
    }

    my $deferring = $self->_is_deferring;
    while (my ($n, $r) = each %{$self->{cache}}) {
      $cached += length($r);
      next if $n+1 <= $.;
      unless ($deferring && $self->{deferred}{$n}) {
        _ci_warn("spurious caching of record $n");
        $good = 0;
      }
    }
    if ($cached != $self->{cached}) {
      _ci_warn("cache size is $self->{cached}, should be $cached");
      $good = 0;
    }
  }

  my (%seen, @duplicate);
  for (@{$self->{lru}}) {
    $seen{$_}++;
    if (not exists $self->{cache}{$_}) {
      _ci_warn("$_ is mentioned in the LRU queue, but not in the cache");
      $good = 0;
    }
  }
  @duplicate = grep $seen{$_}>1, keys %seen;
  if (@duplicate) {
    my $records = @duplicate == 1 ? 'Record' : 'Records';
    my $appear  = @duplicate == 1 ? 'appears' : 'appear';
    _ci_warn("$records @duplicate $appear multiple times in LRU queue: @{$self->{lru}}");
    $good = 0;
  }
  for (keys %{$self->{cache}}) {
    unless (exists $seen{$_}) {
      _ci_warn("record $_ is in the cache but not the LRU queue");
      $good = 0;
    }
  }

  # Now let's check the deferbuffer
  # Unless deferred writing is enabled, it should be empty
  if (! $self->_is_deferring && %{$self->{deferred}}) {
    _ci_warn("deferred writing disabled, but deferbuffer nonempty");
    $good = 0;
  }

  # Every record in the deferbuffer must be present in the readcache
  while (my ($n, $r) = each %{$self->{deferred}}) {
    if (not exists $self->{cache}{$n}) {
      _ci_warn("record $n is in the deferbuffer but is missing from the readcache");
      $good = 0;
    }
    if (! $r) {
      _ci_warn("record $n in the deferbuffer has a false value!");
      $good = 0;
    }
  }

  # Total size of cached data should not exceed the specified limit
  if ($cached > $self->{memory}) {
    _ci_warn("total stored data size is $cached which exceeds the limit of $self->{memory}");
    $good = 0;
  }

  # Stuff related to autodeferment
  if (!$self->{autodefer} && @{$self->{ad_history}}) {
    _ci_warn("autodefer is disabled, but ad_history is nonempty");
    $good = 0;
  }
  if ($self->{autodeferring} && $self->{defer}) {
    _ci_warn("both autodeferring and explicit deferring are active");
    $good = 0;
  }
  if (@{$self->{ad_history}} == 0) {
    # That's OK, no additional tests required
  } elsif (@{$self->{ad_history}} == 2) {
    my @non_number = grep !/^-?\d+$/, @{$self->{ad_history}};
    if (@non_number) {
      my $msg;
      { local $" = ')(';
        $msg = "ad_history contains non-numbers (@{$self->{ad_history}})";
      }
      _ci_warn($msg);
      $good = 0;
    } elsif ($self->{ad_history}[1] < $self->{ad_history}[0]) {
      _ci_warn("ad_history has nonsensical values @{$self->{ad_history}}");
      $good = 0;
    }
  } else {
    _ci_warn("ad_history has bad length <@{$self->{ad_history}}>");
    $good = 0;
  }

  $good;
}

"Cogito, ergo sum.";  # don't forget to return a true value from the file

=head1 NAME

Tie::File - Access the lines of a disk file via a Perl array

=head1 SYNOPSIS

	# This file documents Tie::File version 0.52

	tie @array, 'Tie::File', filename or die ...;

	$array[13] = 'blah';     # line 13 of the file is now 'blah'
	print $array[42];        # display line 42 of the file

	$n_recs = @array;        # how many records are in the file?
	$#array -= 2;            # chop two records off the end


	for (@array) {
	  s/PERL/Perl/g;         # Replace PERL with Perl everywhere in the file
	}

	# These are just like regular push, pop, unshift, shift, and splice
	# Except that they modify the file in the way you would expect

	push @array, new recs...;
	my $r1 = pop @array;
	unshift @array, new recs...;
	my $r1 = shift @array;
	@old_recs = splice @array, 3, 7, new recs...;

	untie @array;            # all finished


=head1 DESCRIPTION

C<Tie::File> represents a regular text file as a Perl array.  Each
element in the array corresponds to a record in the file.  The first
line of the file is element 0 of the array; the second line is element
1, and so on.

The file is I<not> loaded into memory, so this will work even for
gigantic files.

Changes to the array are reflected in the file immediately.

Lazy people and beginners may now stop reading the manual.

=head2 C<recsep>

What is a 'record'?  By default, the meaning is the same as for the
C<E<lt>...E<gt>> operator: It's a string terminated by C<$/>, which is
probably C<"\n">.  (Minor exception: on dos and Win32 systems, a
'record' is a string terminated by C<"\r\n">.)  You may change the
definition of "record" by supplying the C<recsep> option in the C<tie>
call:

	tie @array, 'Tie::File', $file, recsep => 'es';

This says that records are delimited by the string C<es>.  If the file
contained the following data:

	Curse these pesky flies!\n

then the C<@array> would appear to have four elements: 

	"Curse th"
	"e p"
	"ky fli"
	"!\n"

An undefined value is not permitted as a record separator.  Perl's
special "paragraph mode" semantics (E<agrave> la C<$/ = "">) are not
emulated.

Records read from the tied array do not have the record separator
string on the end; this is to allow 

	$array[17] .= "extra";

to work as expected.

(See L<"autochomp">, below.)  Records stored into the array will have
the record separator string appended before they are written to the
file, if they don't have one already.  For example, if the record
separator string is C<"\n">, then the following two lines do exactly
the same thing:

	$array[17] = "Cherry pie";
	$array[17] = "Cherry pie\n";

The result is that the contents of line 17 of the file will be
replaced with "Cherry pie"; a newline character will separate line 17
from line 18.  This means that in particular, this will do nothing:

	chomp $array[17];

Because the C<chomp>ed value will have the separator reattached when
it is written back to the file.  There is no way to create a file
whose trailing record separator string is missing.

Inserting records that I<contain> the record separator string will
produce a reasonable result, but if you can't foresee what this result
will be, you'd better avoid doing this.

=head2 C<autochomp>

Normally, array elements have the record separator removed, so that if
the file contains the text

	Gold
	Frankincense
	Myrrh

the tied array will appear to contain C<("Gold", "Frankincense",
"Myrrh")>.  If you set C<autochomp> to a false value, the record
separator will not be removed.  If the file above was tied with

	tie @gifts, "Tie::File", $gifts, autochomp => 0;

then the array C<@gifts> would appear to contain C<("Gold\n",
"Frankincense\n", "Myrrh\n")>, or (on Win32 systems) C<("Gold\r\n",
"Frankincense\r\n", "Myrrh\r\n")>.

=head2 C<mode>

Normally, the specified file will be opened for read and write access,
and will be created if it does not exist.  (That is, the flags
C<O_RDWR | O_CREAT> are supplied in the C<open> call.)  If you want to
change this, you may supply alternative flags in the C<mode> option.
See L<Fcntl> for a listing of available flags.
For example:

	# open the file if it exists, but fail if it does not exist
	use Fcntl 'O_RDWR';
	tie @array, 'Tie::File', $file, mode => O_RDWR;

	# create the file if it does not exist
	use Fcntl 'O_RDWR', 'O_CREAT';
	tie @array, 'Tie::File', $file, mode => O_RDWR | O_CREAT;

	# open an existing file in read-only mode
	use Fcntl 'O_RDONLY';
	tie @array, 'Tie::File', $file, mode => O_RDONLY;

Opening the data file in write-only or append mode is not supported.

=head2 C<memory>

This is an upper limit on the amount of memory that C<Tie::File> will
consume at any time while managing the file.

Records read in from the file are cached, to avoid having to re-read
them repeatedly.  If you read the same record twice, the first time it
will be stored in the I<cache>, in memory, and the second time it will
be fetched back from the cache.  The amount of data in the cache will
not exceed the value you specified for C<memory>.  If C<Tie::File>
wants to cache a new record, but the cache is full, it will make room
by expiring the least-recently visited records from the cache.

The default memory limit is 2Mib.  You can adjust the maximum
cache size by supplying the C<memory> option.  The argument is the
desired cache size, in bytes.

	# I have a lot of memory, so use a large cache to speed up access
	tie @array, 'Tie::File', $file, memory => 20_000_000;

Setting the memory limit to 0 will inhibit caching; records will be
fetched from disk every time you examine them.

=head2 Option Format

C<-mode> is a synonym for C<mode>.  C<-recsep> is a synonym for
C<recsep>.  C<-memory> is a synonym for C<memory>.  You get the
idea.

=head1 Public Methods

The C<tie> call returns an object, say C<$o>.  You may call 

	$rec = $o->FETCH($n);
	$o->STORE($n, $rec);

to fetch or store the record at line C<$n>, respectively; similarly
the other tied array methods.  (See L<perltie> for details.)  You may
also call the following methods on this object:

=head2 C<flock>

	$o->flock(MODE)

will lock the tied file.  C<MODE> has the same meaning as the second
argument to the Perl built-in C<flock> function; for example
C<LOCK_SH> or C<LOCK_EX | LOCK_NB>.  (These constants are provided by
the C<use Fcntl ':flock'> declaration.)

C<MODE> is optional; the default is C<LOCK_EX>.

C<Tie::File> promises that the following sequence of operations will
be safe:

	my $o = tie @array, "Tie::File", $filename;
	$o->flock;

In particular, C<Tie::File> will I<not> read or write the file during
the C<tie> call.  (Exception: Using C<mode =E<gt> O_TRUNC> will, of
course, erase the file during the C<tie> call.  If you want to do this
safely, then open the file without C<O_TRUNC>, lock the file, and use
C<@array = ()>.)

The best way to unlock a file is to discard the object and untie the
array.  It is probably unsafe to unlock the file without also untying
it, because if you do, changes may remain unwritten inside the object.
That is why there is no shortcut for unlocking.  If you really want to
unlock the file prematurely, you know what to do; if you don't know
what to do, then don't do it.

All the usual warnings about file locking apply here.  In particular,
note that file locking in Perl is B<advisory>, which means that
holding a lock will not prevent anyone else from reading, writing, or
erasing the file; it only prevents them from getting another lock at
the same time.  Locks are analogous to green traffic lights: If you
have a green light, that does not prevent the idiot coming the other
way from plowing into you sideways; it merely guarantees to you that
the idiot does not also have a green light at the same time.

=head2 C<autochomp>

	my $old_value = $o->autochomp(0);    # disable autochomp option
	my $old_value = $o->autochomp(1);    #  enable autochomp option

	my $ac = $o->autochomp();   # recover current value

See L<"autochomp">, above.

=head2 C<defer>, C<flush>, C<discard>, and C<autodefer>

See L<"Deferred Writing">, below.

=head1 Tying to an already-opened filehandle

If C<$fh> is a filehandle, such as is returned by C<IO::File> or one
of the other C<IO> modules, you may use:

	tie @array, 'Tie::File', $fh, ...;

Similarly if you opened that handle C<FH> with regular C<open> or
C<sysopen>, you may use:

	tie @array, 'Tie::File', \*FH, ...;

Handles that were opened write-only won't work.  Handles that were
opened read-only will work as long as you don't try to modify the
array.  Handles must be attached to seekable sources of data---that
means no pipes or sockets.  If you supply a non-seekable handle, the
C<tie> call will try to throw an exception.  (On Unix systems, it
B<will> throw an exception.)

=head1 Deferred Writing

(This is an advanced feature.  Skip this section on first reading.)

Normally, modifying a C<Tie::File> array writes to the underlying file
immediately.  Every assignment like C<$a[3] = ...> rewrites as much of
the file as is necessary; typically, everything from line 3 through
the end will need to be rewritten.  This is the simplest and most
transparent behavior.  Performance even for large files is reasonably
good.

However, under some circumstances, this behavior may be excessively
slow.  For example, suppose you have a million-record file, and you
want to do:

	for (@FILE) {
	  $_ = "> $_";
	}

The first time through the loop, you will rewrite the entire file,
from line 0 through the end.  The second time through the loop, you
will rewrite the entire file from line 1 through the end.  The third
time through the loop, you will rewrite the entire file from line 2 to
the end.  And so on.

If the performance in such cases is unacceptable, you may defer the
actual writing, and then have it done all at once.  The following loop
will perform much better for large files:

	(tied @a)->defer;
	for (@a) {
	  $_ = "> $_";
	}
	(tied @a)->flush;

If C<Tie::File>'s memory limit is large enough, all the writing will
done in memory.  Then, when you call C<-E<gt>flush>, the entire file
will be rewritten in a single pass.

(Actually, the preceding discussion is something of a fib.  You don't
need to enable deferred writing to get good performance for this
common case, because C<Tie::File> will do it for you automatically
unless you specifically tell it not to.  See L<"autodeferring">,
below.)

Calling C<-E<gt>flush> returns the array to immediate-write mode.  If
you wish to discard the deferred writes, you may call C<-E<gt>discard>
instead of C<-E<gt>flush>.  Note that in some cases, some of the data
will have been written already, and it will be too late for
C<-E<gt>discard> to discard all the changes.  Support for
C<-E<gt>discard> may be withdrawn in a future version of C<Tie::File>.

Deferred writes are cached in memory up to the limit specified by the
C<memory> option (see above).  If the cache is full and you try to
write still more deferred data, the cache will discard the oldest
records that do I<not> represent deferred writes, until the cache size
is once again under limit.  If every cached record represents a
deferred write, the cache will be flushed: all cached data will be
written immediately, the cache will be emptied, and the now-empty
space will be used for future deferred writes.

C<push>, C<pop>, C<shift>, C<unshift>, and C<splice> cannot be
deferred.  When you perform one of these operations, any deferred data
is written to the file and the operation is performed immediately.
This may change in a future version.

If you resize the array with deferred writing enabled, the file will
be resized immediately, but deferred records will not be written.

=head2 Autodeferring

C<Tie::File> tries to guess when deferred writing might be helpful,
and to turn it on and off automatically.  In the example above, only
the first two assignments will be done immediately; after this, all
the changes to the file will be deferred up to the user-specified
memory limit.

You should usually be able to ignore this and just use the module
without thinking about deferring.  However, special applications may
require fine control over which writes are deferred, or may require
that all writes be immediate.  To disable the autodeferment feature,
use

	(tied @o)->autodefer(0);

or

       	tie @array, 'Tie::File', $file, autodefer => 0;


=head1 CAVEATS

(That's Latin for 'warnings'.)

=over 4

=item *

This is BETA RELEASE SOFTWARE.  It may have bugs.  See the discussion
below about the (lack of any) warranty.

In particular, this means that the interface may change in
incompatible ways from one version to the next, without warning.  That
has happened at least once already.  The interface will freeze before
Perl 5.8 is released, probably sometime in April 2002.

=item * 

Reasonable effort was made to make this module efficient.  Nevertheless,
changing the size of a record in the middle of a large file will
always be fairly slow, because everything after the new record must be
moved.

=item *

The behavior of tied arrays is not precisely the same as for regular
arrays.  For example:

	# This DOES print "How unusual!"
	undef $a[10];  print "How unusual!\n" if defined $a[10];

C<undef>-ing a C<Tie::File> array element just blanks out the
corresponding record in the file.  When you read it back again, you'll
get the empty string, so the supposedly-C<undef>'ed value will be
defined.  Similarly, if you have C<autochomp> disabled, then

	# This DOES print "How unusual!" if 'autochomp' is disabled
	undef $a[10];  
        print "How unusual!\n" if $a[10];

Because when C<autochomp> is disabled, C<$a[10]> will read back as
C<"\n"> (or whatever the record separator string is.)  

There are other minor differences, particularly regarding C<exists>
and C<delete>, but in general, the correspondence is extremely close.

=item *

Not quite every effort was made to make this module as efficient as
possible.  C<FETCHSIZE> should use binary search instead of linear
search.  The cache's LRU queue should be a heap instead of a list.

The performance of the C<flush> method could be improved.  At present,
it still rewrites the tail of the file once for each block of
contiguous lines to be changed.  In the typical case, this will result
in only one rewrite, but in peculiar cases it might be bad.  It should
be possible to perform I<all> deferred writing with a single rewrite.

These defects are probably minor; in any event, they will be fixed in
a future version of the module.

=item *

The author has supposed that since this module is concerned with file
I/O, almost all normal use of it will be heavily I/O bound, and that
the time to maintain complicated data structures inside the module
will be dominated by the time to actually perform the I/O.  This
suggests, for example, that an LRU read-cache is a good tradeoff, even
if it requires substantial bookkeeping following a C<splice>
operation.

=item *

You might be tempted to think that deferred writing is like
transactions, with C<flush> as C<commit> and C<discard> as
C<rollback>, but it isn't, so don't.

=back

=head1 SUBCLASSING

This version promises absolutely nothing about the internals, which
may change without notice.  A future version of the module will have a
well-defined and stable subclassing API.

=head1 WHAT ABOUT C<DB_File>?

C<DB_File>'s C<DB_RECNO> feature does something similar to
C<Tie::File>, but there are a number of reasons that you might prefer
C<Tie::File>.  C<DB_File> is a great piece of software, but the
C<DB_RECNO> part is less great than the rest of it.

=over 4

=item *

C<DB_File> reads your entire file into memory, modifies it in memory,
and the writes out the entire file again when you untie the file.
This is completely impractical for large files.

C<Tie::File> does not do any of those things.  It doesn't try to read
the entire file into memory; instead it uses a lazy approach and
caches recently-used records.  The cache size is strictly bounded by
the C<memory> option.  DB_File's C<-E<gt>{cachesize}> doesn't prevent
your process from blowing up when reading a big file.

=item *

C<DB_File> has an extremely poor writing strategy.  If you have a
ten-megabyte file and tie it with C<DB_File>, and then use

        $a[0] =~ s/PERL/Perl/;

C<DB_file> will then read the entire ten-megabyte file into memory, do
the change, and write the entire file back to disk, reading ten
megabytes and writing ten megabytes.  C<Tie::File> will read and write
only the first record.

If you have a million-record file and tie it with C<DB_File>, and then
use

        $a[999998] =~ s/Larry/Larry Wall/;

C<DB_File> will read the entire million-record file into memory, do
the change, and write the entire file back to disk.  C<Tie::File> will
only rewrite records 999998 and 999999.  During the writing process,
it will never have more than a few kilobytes of data in memory at any
time, even if the two records are very large.

=item *

Since changes to C<DB_File> files only appear when you do C<untie>, it
can be inconvenient to arrange for concurrent access to the same file
by two or more processes.  Each process needs to call C<$db-E<gt>sync>
after every write.  When you change a C<Tie::File> array, the changes
are reflected in the file immediately; no explicit C<-E<gt>sync> call
is required.  (Or you can enable deferred writing mode to require that
changes be explicitly sync'ed.)

=item *

C<DB_File> is only installed by default if you already have the C<db>
library on your system; C<Tie::File> is pure Perl and is installed by
default no matter what.  Starting with Perl 5.7.3 you can be
absolutely sure it will be everywhere.  You will never have that
surety with C<DB_File>.  If you don't have C<DB_File> yet, it requires
a C compiler.  You can install C<Tie::File> from CPAN in five minutes
with no compiler.

=item *

C<DB_File> is written in C, so if you aren't allowed to install
modules on your system, it is useless.  C<Tie::File> is written in Perl,
so even if you aren't allowed to install modules, you can look into
the source code, see how it works, and copy the subroutines or the
ideas from the subroutines directly into your own Perl program.

=item *

Except in very old, unsupported versions, C<DB_File>'s free license
requires that you distribute the source code for your entire
application.  If you are not able to distribute the source code for
your application, you must negotiate an alternative license from
Sleepycat, possibly for a fee.  Tie::File is under the Perl Artistic
license and can be distributed free under the same terms as Perl
itself.

=back

=head1 AUTHOR

Mark Jason Dominus

To contact the author, send email to: C<mjd-perl-tiefile+@plover.com>

To receive an announcement whenever a new version of this module is
released, send a blank email message to
C<mjd-perl-tiefile-subscribe@plover.com>.

The most recent version of this module, including documentation and
any news of importance, will be available at

	http://perl.plover.com/TieFile/


=head1 LICENSE

C<Tie::File> version 0.52 is copyright (C) 2002 Mark Jason Dominus.

This library is free software; you may redistribute it and/or modify
it under the same terms as Perl itself.

These terms are your choice of any of (1) the Perl Artistic Licence,
or (2) version 2 of the GNU General Public License as published by the
Free Software Foundation, or (3) any later version of the GNU General
Public License.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this library program; it should be in the file C<COPYING>.
If not, write to the Free Software Foundation, Inc., 59 Temple Place,
Suite 330, Boston, MA 02111 USA

For licensing inquiries, contact the author at:

	Mark Jason Dominus
	255 S. Warnock St.
	Philadelphia, PA 19107

=head1 WARRANTY

C<Tie::File> version 0.52 comes with ABSOLUTELY NO WARRANTY.
For details, see the license.

=head1 THANKS

Gigantic thanks to Jarkko Hietaniemi, for agreeing to put this in the
core when I hadn't written it yet, and for generally being helpful,
supportive, and competent.  (Usually the rule is "choose any one.")
Also big thanks to Abhijit Menon-Sen for all of the same things.

Special thanks to Craig Berry and Peter Prymmer (for VMS portability
help), Randy Kobes (for Win32 portability help), Clinton Pierce and
Autrijus Tang (for heroic eleventh-hour Win32 testing above and beyond
the call of duty), Michael G Schwern (for testing advice), and the
rest of the CPAN testers (for testing generally).

Additional thanks to:
Edward Avis /
Gerrit Haase /
Nikola Knezevic /
Nick Ing-Simmons /
Tassilo von Parseval /
H. Dieter Pearcey /
Slaven Rezic /
Peter Scott /
Peter Somu /
Autrijus Tang (again) /
Tels

=head1 TODO

More tests.  (_twrite should be tested separately, because there are a
lot of weird special cases lurking in there.)

Improve SPLICE algorithm to use deferred writing machinery.

More tests.  (Stuff I didn't think of yet.)

Paragraph mode?

Fixed-length mode.  Leave-blanks mode.

Maybe an autolocking mode?

Record locking with fcntl()?  Then the module might support an undo
log and get real transactions.  What a tour de force that would be.  

Replace LRU list with heap structure.

Cleverer strategy for flushing deferred writes.

Binary instead of linear search in FETCHSIZE.

Statistics-gathering facility.

Refactor cache management functions.  Clean up SPLICE.

More tests.

=cut

