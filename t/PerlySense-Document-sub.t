#!/usr/bin/perl -w
use strict;

use Test::More tests => 46;
use Test::Exception;

use File::Basename;
use File::Spec::Functions;

use lib "../lib";

use_ok("Devel::PerlySense::Document");
use_ok("Devel::PerlySense::Document::Location");


BEGIN { -d "t" and chdir("t"); }


ok(my $oDocument = Devel::PerlySense::Document->new(oPerlySense => Devel::PerlySense->new()), "new ok");

my $dirData = "data/simple-lib";
my $fileOrigin = "$dirData/lib/Win32/Word/Writer.pm";
my $oLocation;

ok($oDocument->parse(file => $fileOrigin), "Parsed file ok");
is(my $package = $oDocument->packageAt(row => 429), "Win32::Word::Writer", "Correct package Table ok");

is($oDocument->oLocationSub(name => "Write", package => "missing package"), undef, "Didn't find missing package declaration");
ok($oLocation = $oDocument->oLocationSub(name => "Write", package => $package), "Found correct declaration");
is($oLocation->file, $fileOrigin, " Got file");
is($oLocation->row, 396, "  row");
is($oLocation->col, 1, "  col");

ok($oLocation = $oDocument->oLocationSub(name => "main_sub"), "Found correct declaration in default package main");
is($oLocation->file, $fileOrigin, " Got file");
is($oLocation->row, 132, "  row");
is($oLocation->col, 1, "  col");

ok($oLocation = $oDocument->oLocationSub(name => "NewParagraph", package => $package), "Found correct declaration");
is($oLocation->file, $fileOrigin, " Got file");
is($oLocation->row, 446, "  row");
is($oLocation->col, 1, "  col");





ok($oDocument = Devel::PerlySense::Document->new(oPerlySense => Devel::PerlySense->new()), "new ok");
$fileOrigin = "$dirData/lib/Game/Event/Timed.pm";

ok($oDocument->parse(file => $fileOrigin), "Parsed file ok");
ok($oLocation = $oDocument->oLocationSubDefinition(name => "checkTick", row => 107), "Found sub from col package");
is($oLocation->file, $fileOrigin, " Got file");
is($oLocation->row, 123, "  row");
is($oLocation->col, 1, "  col");


ok($oLocation = $oDocument->oLocationSubDefinition(name => "checkTick", row => 1), "Found sub from col package main");
is($oLocation->file, $fileOrigin, " Got file");
is($oLocation->row, 21, "  row");
is($oLocation->col, 1, "  col");



ok($oLocation = $oDocument->oLocationSubDefinition(name => "checkTick", package => "main"), "Found sub from param package main");
is($oLocation->file, $fileOrigin, " Got file");
is($oLocation->row, 21, "  row");
is($oLocation->col, 1, "  col");


ok($oLocation = $oDocument->oLocationSubDefinition(name => "checkTick"), "Found sub from default package main");
is($oLocation->file, $fileOrigin, " Got file");
is($oLocation->row, 21, "  row");
is($oLocation->col, 1, "  col");


ok($oLocation = $oDocument->oLocationSubDefinition(name => "checkTick", package => "Game::Event::Timed"), "Found sub from default package main");
is($oLocation->file, $fileOrigin, " Got file");
is($oLocation->row, 123, "  row");
is($oLocation->col, 1, "  col");






print "\n*** Parent modules\n";

$dirData = "data/project-lib";
my $rexFileDest = qr/Game.Object.Worm.pm/;

ok($oDocument = Devel::PerlySense::Document->new(oPerlySense => Devel::PerlySense->new()), "new ok");
$fileOrigin = "$dirData/Game/Object/Worm/Bot.pm";

ok($oDocument->parse(file => $fileOrigin), "Parsed file ok");
ok($oLocation = $oDocument->oLocationSubDefinition(name => "loadFile", package => "Game::Object::Worm::Bot"), "Found sub in parent package");
like($oLocation->file, $rexFileDest, " Got file");
is($oLocation->row, 355, "  row");
is($oLocation->col, 1, "  col");




__END__
