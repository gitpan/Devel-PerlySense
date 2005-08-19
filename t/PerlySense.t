#!/usr/bin/perl -w
use strict;
use Test::More tests => 5;
use Test::Exception;

use File::Basename;
use File::Spec::Functions;

use lib "../lib";



use_ok("Devel::PerlySense");

ok(my $oPs = Devel::PerlySense->new(), "new ok");


is($oPs->fileFromModule("Foo"), "Foo.pm", "fileFromModule ok");
is($oPs->fileFromModule("Foo::Bar"), catfile("Foo", "Bar") . ".pm", "fileFromModule ok");
is($oPs->fileFromModule("Foo::Bar::Baz"), catfile("Foo", "Bar", "Baz") . ".pm", "fileFromModule ok");



__END__
