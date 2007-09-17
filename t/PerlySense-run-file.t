#!/usr/bin/perl -w
use strict;

use Test::More tests => 7;
use Test::Exception;

use Data::Dumper;

use lib "../lib";

use_ok("Devel::PerlySense");


BEGIN { -d "t" and chdir("t"); }



ok(
    my $oPerlySense = Devel::PerlySense->new(),
    "New PerlySense object ok",
);
            


diag("Run test file inside dir");
{

    my $dirBase = "data/project/with-dir";
    my $dirProject = "$dirBase/source";
    my $dirTest = "$dirProject/t";
    my $fileTest = "$dirTest/Game-Lawn.t";
    
    ok(
        my $rhRun = $oPerlySense->rhRunFile(file => $fileTest),
        "rhRunFile returned a data structure",
    );
    is(scalar keys %$rhRun, 3, "  correct item count");
    is($rhRun->{type_source_file}, "Test", "    type_source_file");
    is($rhRun->{command_run}, q|prove -v "-I." "-Ilib" "t\Game-Lawn.t"|, "    command_run");
    like($rhRun->{dir_run_from}, qr|t.data.project.with-dir.source|, "    dir_run_from");
    
}




__END__
