#!/usr/bin/perl -w
use strict;

use lib "lib";
use Devel::PerlySense;


main();

sub main {
    my $version = Devel::PerlySense->VERSION;
    
    -e "../../branches/release-$version" and
            die("The version ($version) seems to be released already\n");

    -f "Build" and system("perl Build realclean");
    sys("perl Build.PL") and die;
    sys("perl Build distcheck") and die;
    sys("perl Build manifest") and die;
    sys("perl Build disttest") and die;
    sys("perl Build dist") and die;

    my $release_tar_file = "Devel-PerlySense-$version.tar.gz";
    rename($release_tar_file, "../release/$release_tar_file");

    sys("perl Build realclean") and die;

    say("Adding file");
    sys("svn add ../release/$release_tar_file") and die;
    say("Committing file");
    sys(qq{svn commit -m "Tarball for release ($version)" ../release/$release_tar_file}) and die;

    chdir("../..");  # above trunk

    my ($svn_root) = grep { /URL:/ } `svn info`;
    chomp($svn_root);
    $svn_root =~ s/URL: //;

    my $release = "release-$version";
    my $release_branch = "branches/release-$version";

    sys(qq{svn cp -m "Branched for release ($version)" $svn_root/trunk $svn_root/$release_branch});
#    sys(qq{svn ci  $svn_root/trunk $svn_root/$release_branch});

    sys(qq{svn co $svn_root/$release_branch $release_branch});

    
}



sub pause { <STDIN>; }

sub say { print @_, "\n"; }

sub sys {
    my ($command) = @_;

    say("SYS($command)");
    system($command) and die("Could not run ($command)\n");
}


__END__
