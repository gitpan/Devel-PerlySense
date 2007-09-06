#!/usr/bin/perl -w
use strict;

use Test::More tests => 10;
use Test::Exception;
#use Test::Differences;

use Data::Dumper;


use lib "../lib";

use_ok("Devel::PerlySense");
use_ok("Devel::PerlySense::Class");
use_ok("Devel::PerlySense::Editor::Emacs");


BEGIN { -d "t" and chdir("t"); }


throws_ok(
    sub { Devel::PerlySense::Editor::Emacs->new(
    ) },
    qr/oPerlySense/,
    "new fails ok with missing name",
);

lives_ok(
    sub { Devel::PerlySense::Editor::Emacs->new(
        oPerlySense => Devel::PerlySense->new(),
    ) },
    "new ok with name",
);





ok(my $oPerlySense = Devel::PerlySense->new(), "Created PerlySense object ok");
ok(
    my $oEditor = Devel::PerlySense::Editor::Emacs->new(
        oPerlySense => $oPerlySense
    ),
    "Created Editor ok",
);

my $dirData = "data/project-lib";
my $fileOrigin = "$dirData/Game/Object/Worm/ShaiHulud.pm";

ok(
    my $oClassOjectWormShai = Devel::PerlySense::Class->newFromFileAt(
        oPerlySense => $oPerlySense,
        file => $fileOrigin,
        row => 20,
        col => 1,
    ),
    "newFromFileAt at proper package location ok",
);


ok(
    my $textShai = $oEditor->classOverview(oClass => $oClassOjectWormShai),
    " render classOverview ok",
);
#warn("-----\n$textShai\n-----\n");
my $s = " ";
my $sNone = "       ";
my $textExpected = qq{* Inheritance *
[ Game::Object                  ] <-----+
[ Game::Object::Worm            ]       |
[<Game::Object::Worm::ShaiHulud>] --> [ Game::Lawn ]

* NeighbourHood *
[ Game::Object::Prize ] [ Game::Object::Worm::Bot       ] -none-
[ Game::Object::Wall  ] [<Game::Object::Worm::ShaiHulud>]$sNone
[ Game::Object::Worm  ] [ Game::Object::Worm::Shaitan   ]$sNone

};

#eq_or_diff
is($textShai, $textExpected, "  And got correct output");






__END__


+-------------------------------+
[         Game::Object          ] <-----+
+-------------------------------+       |
  ^                                     |
  |                                     |
  |                                     |
+-------------------------------+       |
[      Game::Object::Worm       ]       |
+-------------------------------+       |
  ^                                     |
  |                                     |
  |                                     |
+-------------------------------+     +------------+
[ Game::Object::Worm::ShaiHulud ] --> [ Game::Lawn ]
+-------------------------------+     +------------+



.................................
:         Game::Object          : <-----+
:...............................:       |
  ^                                     |
  |                                     |
  |                                     |
.................................       |
:      Game::Object::Worm       :       |
:...............................:       |
  ^                                     |
  |                                     |
  |                                     |
.................................     ..............
: Game::Object::Worm::ShaiHulud : --> : Game::Lawn :
:...............................:     :............:



