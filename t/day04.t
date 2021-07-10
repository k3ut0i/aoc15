# -*- mode: perl; cpel-indent-level: 4 -*-
use strict;
use warnings;
use Test::More;
use_ok('Day4');
use lib '../src';
use Day4;

is(Day4::find_num("abcdef"), 609043);
is(Day4::find_num("pqrstuv"), 1048970);

done_testing();
