#!/usr/bin/perl -w
use strict;

use Test::More tests => 14;
use Test::Exception;

use Data::Dumper;


use lib "lib";

use_ok("Devel::PerlySense");
use_ok("Devel::PerlySense::BookmarkConfig");



ok(my $oPerlySense = Devel::PerlySense->new(), "new PerlySense");


diag("Bad bookmark config");

$oPerlySense->rhConfig->{bookmark} = [ {
    moniker => "",
    rex => "",
}, ];
throws_ok(
    sub { $oPerlySense->oBookmarkConfig->raDefinition },
    qr/Bad Bookmark definition: No 'moniker' specified'/,
    "Missing moniker dies ok",
);

$oPerlySense->rhConfig->{bookmark} = [ {
    moniker => "Broken",
    rex => "fds/",
}, ];
throws_ok(
    sub { $oPerlySense->oBookmarkConfig->raDefinition },
    qr/syntax error/i,
    "Bad Perl syntax dies ok",
);

$oPerlySense->rhConfig->{bookmark} = [ {
    moniker => "Broken",
    rex => "'not a rex object'",
}, ];
throws_ok(
    sub { $oPerlySense->oBookmarkConfig->raDefinition },
    qr/doesn't result in a regex/i,
    "Not a rex dies ok",
);




diag("Proper bookmark config");
$oPerlySense->rhConfig->{bookmark} = [
    {
        moniker => "Todo",
        rex => 'qr/\# \s* TODO: \s* ( .+ )/x',
    },
    {
        moniker => "Debugging",
        rex => [
            'qr/DB::single/',
            'qr/debug\(/x',
        ],
    },
];


ok(my $oBookmarkConfig = $oPerlySense->oBookmarkConfig, "  BookmarkConfig");
isa_ok($oBookmarkConfig, "Devel::PerlySense::BookmarkConfig");



is(scalar @{$oBookmarkConfig->raDefinition}, 2, "  found 2 definitions");

ok(my $todo_definition = $oBookmarkConfig->raDefinition->[0], "Got Todo");
isa_ok($todo_definition, "Devel::PerlySense::Bookmark::Definition");






my $dirData = "t/data/project-lib";
my $fileOrigin = "$dirData/Game/Object/Worm/ShaiHulud.pm";

my @aMatchResult;

diag("Find matches");


throws_ok(
    sub { $oBookmarkConfig->aMatchResult(file => "missing_file.pm") },
    qr/Could not read source file/,
);



@aMatchResult = $oBookmarkConfig->aMatchResult(file => $fileOrigin);
is(scalar @aMatchResult, 2, "Found correct number of matching definitions");

my $oMatchResult = $aMatchResult[0];
my @aMatch = @{$oMatchResult->raMatch};
is(scalar @aMatch, 3, "  Found the correct number of matches");




__END__
