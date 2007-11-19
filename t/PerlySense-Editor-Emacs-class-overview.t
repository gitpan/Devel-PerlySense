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

* Uses *
[ Carp ] [ Class::MethodMaker ] [ Data::Dumper ]

* NeighbourHood *
[ Game::Object::Prize ] [ Game::Object::Worm::Bot       ] -none-
[ Game::Object::Wall  ] [<Game::Object::Worm::ShaiHulud>]$sNone
[ Game::Object::Worm  ] [ Game::Object::Worm::Shaitan   ]$sNone

* Bookmarks *
- Todo
ShaiHulud.pm:76: Fix something here
ShaiHulud.pm:127: Find a Prize
ShaiHulud.pm:134: Turn
- Note
ShaiHulud.pm:96:         #XXX fix before checkin

* Structure *
==;"";;;;===;==S{;;;;";;;;}=S{;;{;'{;;";};}";}=S{;{";";";;'
{;;";};}";};
/;

    eq_or_diff
#    is
            ($textShai, $textExpected, "  And got correct output");

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

* Uses *
[ Class::MethodMaker ] [ Game::Event::Timed ]
[ Data::Dumper       ] [ Game::Location     ]

* NeighbourHood *
-none- [ Game::Application ] [ Game::Object::Prize ]
       [ Game::Controller  ] [ Game::Object::Wall  ]
       [ Game::Direction   ] [ Game::Object::Worm  ]
       [ Game::Lawn        ] $textGameObjectSpace
       [ Game::Location    ] $textGameObjectSpace
       [<Game::Object     >] $textGameObjectSpace
       [ Game::UI          ] $textGameObjectSpace

* Bookmarks *

* Structure *
==;;;;;==;=;=;=;=;=;==S{;;;;";;;;;;;}=S{;;{;;}";;};
/;

    eq_or_diff
    #is
    ($textShai, $textExpected, "  And got correct output");

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





