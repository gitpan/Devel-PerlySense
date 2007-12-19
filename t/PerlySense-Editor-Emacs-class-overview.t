#!/usr/bin/perl -w
use strict;

use Test::More tests => 13;
use Test::Exception;
use Test::Differences;

use Data::Dumper;


use lib "lib";

use_ok("Devel::PerlySense");
use_ok("Devel::PerlySense::Class");
use_ok("Devel::PerlySense::Editor::Emacs");



throws_ok(
    sub { Devel::PerlySense::Editor::Emacs->new(
    ) },
    qr/oPerlySense/,
    "new fails ok with missing name",
);

lives_ok(
    sub { Devel::PerlySense::Editor::Emacs->new(
        oPerlySense => Devel::PerlySense->new(),
        widthDisplay => undef,
    ) },
    "new ok with name",
);




ok(my $oPerlySense = Devel::PerlySense->new(), "Created PerlySense object ok");
ok(
    my $oEditor = Devel::PerlySense::Editor::Emacs->new(
        oPerlySense => $oPerlySense,
        widthDisplay => 59,
    ),
    "Created Editor ok",
);
$oEditor->widthDisplay(70);


my $s = " ";
my $sNone = "       ";


{
    my $dirData = "t/data/project-lib";
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

    $oPerlySense->setFindProject(file => $fileOrigin);
    local $oPerlySense->rhConfig->{bookmark} = [
        {
            moniker => "Todo",
            rex => 'qr/\# \s* TODO: \s* ( .+ )/x',
        },
        {
            moniker => "Note",
            rex => 'qr/\# \s* XXX \s* .+/x',
        },
    ];


    ok(
        my $textShai = $oEditor->classOverview(oClass => $oClassOjectWormShai),
        " render classOverview ok",
    );
    #warn("-----\n$textShai\n-----\n");
    my $textExpected = qq/* Inheritance *
[ Game::Object                  ] <-----+
[ Game::Object::Worm            ]       |
[<Game::Object::Worm::ShaiHulud>] --> [ Game::Lawn ]

* API *
\\>END                               \\>oLocation
\\>awardScorePoints                  \\>oLocationRandom
\\>buildBodyRight                    \\>oPlacePrize
\\>checkTick                         \\>oPlaceWorm
\\>color                             \\>oUI
\\>crash                             \\>oValidLocationAfterMove
\\>grow                              \\>objectHasMoved
\\>height                            ->oppositeDirection
\\>isAnythingAt                      \\>placeObjectAt
\\>isAnythingBlockingAt              \\>placeObjectBodyPartAt
\\>isBlocking                        ->possiblyTurnRandomly
\\>isLocationOnLawn                  ->possiblyTurnTowardsPrize
\\>isLocationValidForMove            \\>prizeWasClaimedBy
\\>isLocationValidForPlacement       ->probabilityTurnRandomly
\\>isObjectAt                        ->probabilityTurnTowardsPrize
\\>isObjectLocationValidForPlacement \\>raBodyChar
\\>isRealPlayer                      \\>raBodyLocation
\\>lengthActual                      ->randomDirection
\\>lengthIdeal                       \\>removeObject
\\>loadFile                          \\>removeObjectBodyPartAt
\\>moveForward                       \\>rhGrid
->new                               \\>rhPrize
\\>oController                       \\>score
\\>oDirection                        \\>turn
\\>oDirectionToPrize                 \\>width
\\>oEventMove                        \\>wormHasCrashed
\\>oLawn

* Bookmarks *
- Todo
ShaiHulud.pm:76: Fix something here
ShaiHulud.pm:127: Find a Prize
ShaiHulud.pm:134: Turn
- Note
ShaiHulud.pm:96:         #XXX fix before checkin

* Uses *
[ Carp ] [ Class::MethodMaker ] [ Data::Dumper ]

* NeighbourHood *
[ Game::Object::Prize ] [ Game::Object::Worm::Bot       ] -none-
[ Game::Object::Wall  ] [<Game::Object::Worm::ShaiHulud>]
[ Game::Object::Worm  ] [ Game::Object::Worm::Shaitan   ]/;
    eq_or_diff
#    is
            ($textShai, $textExpected, "  And got correct output");

# * Structure *
# ==;"";;;;===;==S{;;;;";;;;}=S{;;{;'{;;";};}";}=S{;{";";";;'
# {;;";};}";};


}



{
    my $dirData = "t/data/project-lib";
    my $fileOrigin = "$dirData/Game/Object.pm";

    ok(
        my $oClassOject = Devel::PerlySense::Class->newFromFileAt(
            oPerlySense => $oPerlySense,
            file => $fileOrigin,
            row => 1,
            col => 1,
        ),
        "newFromFileAt at proper package location ok",
    );


    ok(
        my $textShai = $oEditor->classOverview(oClass => $oClassOject),
        " render classOverview ok",
    );
    #warn("-----\n$textShai\n-----\n");

    my $textGameObjectSpace = "                       ";
    my $textExpected = qq/* Inheritance *
[<Game::Object>]

* API *
->buildBodyRight ->isBlocking ->oLawn     ->raBodyChar
->color          ->new        ->oLocation ->raBodyLocation

* Bookmarks *

* Uses *
[ Class::MethodMaker ] [ Game::Event::Timed ]
[ Data::Dumper       ] [ Game::Location     ]

* NeighbourHood *
-none- [ Game::Application ] [ Game::Object::Prize ]
       [ Game::Controller  ] [ Game::Object::Wall  ]
       [ Game::Direction   ] [ Game::Object::Worm  ]
       [ Game::Lawn        ]
       [ Game::Location    ]
       [<Game::Object     >]
       [ Game::UI          ]/;

    eq_or_diff
    #is
    ($textShai, $textExpected, "  And got correct output");

# * Structure *
# ==;;;;;==;=;=;=;=;=;==S{;;;;";;;;;;;}=S{;;{;;}";;};


}





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





