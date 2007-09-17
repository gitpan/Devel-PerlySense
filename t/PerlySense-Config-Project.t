#!/usr/bin/perl -w
use strict;

use Test::More tests => 16;
use Test::Exception;

use File::Path;

use Data::Dumper;

use lib "../lib";

use_ok("Devel::PerlySense::Config::Project");
use_ok("Devel::PerlySense");





ok(
    my $oPerlySense = Devel::PerlySense->new(),
    "New PerlySense object ok",
);


my $dir = "t/data/config";
my $dirTemp = "$dir/temp";

diag("Creating temp dir");
mkpath($dirTemp);
ok(-e $dirTemp, "Temp dir created ok");
END {
    diag("Removing  temp dir");
    rmtree($dirTemp);
    ok( ! -e $dirTemp, "Temp file gone");
}



ok(
    my $oConfig = Devel::PerlySense::Config::Project->new(),
    "Created config in temp dir ok",
);



is_deeply($oConfig->rhConfig, {}, "Empty config");

throws_ok(
    sub {
        $oConfig->loadConfig(dirRoot => $dirTemp);
    },
    qr/Could not open config file .t.data.config.temp..PerlySenseProject.project.cfg./,
    "Can't load nonexisting config file ok",
);

is_deeply($oConfig->rhConfig, {}, "  Empty config");
is($oConfig->dirRoot, undef, "  No dirRoot set");


ok( ! -e "dirTemp/.PerlySenseProject", "No project dir");
ok(
    $oConfig->createFileConfigDefault(dirRoot => $dirTemp),
    "Created new project config",
);
like(
    $oConfig->dirRoot,
    qr/t.data.config.temp$/,
    "dirRoot set to the new location",
);
ok(-e "$dirTemp/.PerlySenseProject", "Project dir created");
ok(-e "$dirTemp/.PerlySenseProject/project.cfg", "Project config file created");
is(scalar keys %{$oConfig->rhConfig}, 2, "  Loaded config");




                            


__END__
