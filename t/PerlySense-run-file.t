#!/usr/bin/perl -w
use strict;

use Test::More tests => 34;
use Test::Exception;

use Data::Dumper;

use lib "../lib";

use_ok("Devel::PerlySense");


BEGIN { -d "t" and chdir("t"); }




my $dot = "[.]";
my $up = "${dot}${dot}";



diag("Identify which file type to run");
{
    my $dirBase = "data/project/with-perlysenseproject";
    my $dirProject = "$dirBase/source";
    my $dirTest = "$dirProject/bogus/t";
    my $fileTest = "$dirTest/Game-Lawn.t";


    ok(
        my $oPerlySense = Devel::PerlySense->new(),
        "New PerlySense object ok",
    );
    ok($oPerlySense->setFindProject(file => $fileTest), "Found Project");
    ok(my $oProject = $oPerlySense->oProject, "  got project property");


    diag("  Bad config formats");
    {
        local $oProject->rhConfig->{run_file}->[0]->{rex} = undef;
        
        throws_ok(
            sub { $oProject->rhConfigTypeForFile(file => $fileTest) },
            qr/Missing rex key/,
            "Missing regex found ok",
        );
    }

    {
        local $oProject->rhConfig->{run_file}->[0]->{rex} = 'abc(';
        
        throws_ok(
            sub { $oProject->rhConfigTypeForFile(file => $fileTest) },
            qr/Invalid rex value in config/,
            "Invalid regex found ok",
        );
    }


    
    {
        local $oProject->rhConfig->{run_file} = [];
        
        throws_ok(
            sub { $oProject->rhConfigTypeForFile(file => $fileTest) },
            qr/No run_perl rex matched the/,
            "No matching type found ok",
        );
    }
    

    my $rhConfigType;

    ok($rhConfigType = $oProject->rhConfigTypeForFile(file => "abc.t"), "Identify a .t file");
    is($rhConfigType->{moniker}, "Test", "  correct moniker");
   
    ok($rhConfigType = $oProject->rhConfigTypeForFile(file => "abc.pm"), "Identify a .pm file");
    is($rhConfigType->{moniker}, "Module", "  correct moniker");
   
    ok($rhConfigType = $oProject->rhConfigTypeForFile(file => "abc.pl"), "Identify a .pl file");
    is($rhConfigType->{moniker}, "Script", "  correct moniker");
   
    ok($rhConfigType = $oProject->rhConfigTypeForFile(file => "abc"), "Identify everything else");
    is($rhConfigType->{moniker}, "Script (no .pl)", "  correct moniker");

}






diag("Run test file inside dir");
{    
    #This is to avoid identifying the .PerlySenseProject directory
    #_of_the_development_project_ to interfere with the test which
    #expects a free way all the way up to the root without any
    #projects.
    no warnings;
    local *Devel::PerlySense::Project::newFindExplicit = sub {
        undef;
    };

    
    my $dirBase = "data/project/with-dir";
    my $dirProject = "$dirBase/source";
    my $dirTest = "$dirProject/t";
    my $fileTest = "$dirTest/Game-Lawn.t";


    ok(
        my $oPerlySense = Devel::PerlySense->new(),
        "New PerlySense object ok",
    );    
    ok(
        my $rhRun = $oPerlySense->rhRunFile(file => $fileTest),
        "rhRunFile returned a data structure",
    );
    is(scalar keys %$rhRun, 3, "  correct item count");
    is($rhRun->{type_source_file}, "Test", "    type_source_file");
    like(
        $rhRun->{command_run},
        qr|prove -v "-I." "-Ilib" "t.Game-Lawn.t"|,
        "    command_run",
    );
    like($rhRun->{dir_run_from}, qr|t.data.project.with-dir.source|, "    dir_run_from");

}




diag("Run test .pm file inside dir with config");
{
    my $dirBase = "data/project/with-perlysenseproject";
    my $dirProject = "$dirBase/source";
    my $dirTest = "$dirProject/bogus/t";
    my $fileTest = "$dirTest/Game-Lawn.t";


    ok(
        my $oPerlySense = Devel::PerlySense->new(),
        "New PerlySense object ok",
    );
    ok(
        my $rhRun = $oPerlySense->rhRunFile(file => $fileTest),
        "rhRunFile returned a data structure",
    );
    is_deeply(
        $oPerlySense->rhConfig->{project}->{inc_dir},
        [
            "glib/perl5lib",
            "deps/perl5lib",
        ],
        "Correct project config with inc dir found",
    );
    is(scalar keys %$rhRun, 3, "  correct item count");
    is($rhRun->{type_source_file}, "Test", "    type_source_file");
    like(
        $rhRun->{command_run},
        qr|prove -v "-Iglib.perl5lib" "-Ideps.perl5lib" "-I." "-Ilib" "bogus.t.Game-Lawn.t"|,
        "    command_run",
    );
    like(
        $rhRun->{dir_run_from},
        qr|project.with-perlysenseproject.source|,
        "    dir_run_from is project root",
    );

}





diag("Run test .pl file inside dir with config");
{
    my $dirBase = "data/project/with-perlysenseproject";
    my $dirProject = "$dirBase/source";
    my $dirBin = "$dirProject/bogus/bin";
    my $fileBin = "$dirBin/worms.pl";

    ok(
        my $oPerlySense = Devel::PerlySense->new(),
        "New PerlySense object ok",
    );
    ok(
        my $rhRun = $oPerlySense->rhRunFile(file => $fileBin),
        "rhRunFile returned a data structure",
    );
    is_deeply(
        $oPerlySense->rhConfig->{project}->{inc_dir},
        [
            "glib/perl5lib",
            "deps/perl5lib",
        ],
        "Correct project config with inc dir found",
    );
    is($rhRun->{type_source_file}, "Script", "    type_source_file");
    like(
        $rhRun->{command_run},
        qr|perl "-I${up}.${up}.glib.perl5lib" "-I${up}.${up}.deps.perl5lib" "-I${up}.${up}" "-I${up}.${up}.lib" "worms.pl"|,
        "    command_run has relative paths for includes",
    );
    like(
        $rhRun->{dir_run_from},
        qr|project.with-perlysenseproject.source.bogus.bin|,
        "    dir_run_from is file dir",
    );

}




__END__
