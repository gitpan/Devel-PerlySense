#!/usr/bin/perl -w
use strict;

use Test::More tests => 12;
use Test::Exception;

use Data::Dumper;
use File::Basename;
use File::Spec::Functions;

use lib "../lib";

use_ok("Devel::PerlySense");
use_ok("Devel::PerlySense::Document");
use_ok("Devel::PerlySense::Document::Meta");


BEGIN { -d "t" and chdir("t"); }


my $dirData = "data/project-lib";
my $oMeta;


{
    my $fileOrigin = "$dirData/Game/Object/Worm/Bot.pm";
    ok(my $oDocument = Devel::PerlySense::Document->new(oPerlySense => Devel::PerlySense->new()), "new ok");
    
    ok($oDocument->parse(file => $fileOrigin), "Parsed file ok");
    
    $oMeta = $oDocument->oMeta;
    is_deeply([sort @{$oMeta->raNameModuleBase}], [
        sort qw/
                Game::Object::Worm
                /], " correct used modules");
}



{
    my $fileOrigin = "$dirData/Game/Object/Worm/ShaiHulud.pm";
    ok(my $oDocument = Devel::PerlySense::Document->new(oPerlySense => Devel::PerlySense->new()), "new ok");
    
    ok($oDocument->parse(file => $fileOrigin), "Parsed file ok");
    
    $oMeta = $oDocument->oMeta;
    is_deeply([sort @{$oMeta->raNameModuleBase}], [
        sort qw/
                Game::Object::Worm
                Game::Lawn
                /], " correct used modules");
}



{
    my $fileOrigin = "$dirData/Game/Object/Worm/Shaitan.pm";
    ok(my $oDocument = Devel::PerlySense::Document->new(oPerlySense => Devel::PerlySense->new()), "new ok");
    
    ok($oDocument->parse(file => $fileOrigin), "Parsed file ok");
    
    $oMeta = $oDocument->oMeta;
    is_deeply([sort @{$oMeta->raNameModuleBase}], [
        sort qw/
                Game::Lawn
                Game::Object::Worm
                /], " correct used modules");
}




__END__
