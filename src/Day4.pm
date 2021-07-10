# -*- mode: cperl; cperl-indent-level: 4 -*-
#!/usr/bin/perl
package Day4;

use strict;
use warnings;

use Digest::MD5 qw(md5_hex);

sub find_num {
  my ($prefix, $num_zeroes) = @_;
  my $num = 0; my $d = '';
  $num_zeroes //= 5; my $zero_str = '0' x $num_zeroes;
  while ($d !~ /^$zero_str/) {
      $num++;
      $d = md5_hex($prefix.$num);
  }
  return $num;
}

1;
